namespace Fhusl

module Convert = 
    open System

    let m = [|
        [|3.240969941904521; -1.537383177570093; -0.498610760293|];
        [|-0.96924363628087; 1.87596750150772; 0.041555057407175|];
        [|0.055630079696993; -0.20397695888897; 1.056971514242878|];
    |]

    let m_inv = [|
        [|0.41239079926595; 0.35758433938387; 0.18048078840183|];
        [|0.21263900587151; 0.71516867876775; 0.072192315360733|];
        [|0.019330818715591; 0.11919477979462; 0.95053215224966|];
    |]

    let refX = 0.95045592705167
    let refY = 1.0
    let refZ = 1.089057750759878
    let refU = 0.19783000664283
    let refV = 0.46831999493879
    let kappa = 903.2962962
    let epsilon = 0.0088564516
    let degrees rad = 
        rad * (180.0 / Math.PI)
    let radians deg = 
        deg * (Math.PI / 180.0)

    let get_bounds L =
        let sub1 = ((L + 16.0) ** 3.0) / 1560896.0
        let sub2 = if sub1 > epsilon then sub1 else L / kappa
        m |> Array.map(fun ms ->
            [|0..1|] |> Array.map(fun t -> 
            let m1, m2, m3 = ms.[0], ms.[1], ms.[2]
            let top1 = (284517.0 * m1 - 94839.0 * m3) * sub2
            let top2 = (838422.0 * m3 + 769860.0 * m2 + 731718.0 * m1) * L * sub2 - 769860.0 * float(t) * L
            let bottom = (632260.0 * m3 - 126452.0 * m2) * sub2 + 126452.0 * float(t)
            (top1 / bottom, top2 / bottom)
        )) |> Array.collect (fun elem -> elem) // took a while to get right Array.map constructs and find Array.collect



    // had problems getting the right signature on this one - had used fst and snd with only two args for a while
    let intersect_line_line ((x1,y1), (x2,y2)) =
        (y1 - y2) / (x2 - x1)

    let distance_from_pole (x,y) =
        Math.Sqrt(Math.Pow(x, 2.0) + Math.Pow(y, 2.0))

    let length_of_ray_until_intersect (theta, line) =
        let m1, b1 = line
        let length = b1 / (Math.Sin(theta) - m1 * Math.Cos(theta))
        if length < 0.0 then
            None
        else
            Some(length)

    let max_safe_chroma_for_L L =
        get_bounds L
            |> Array.map(fun line ->
                let m1, b1 = line
                let x = intersect_line_line((m1, b1), (-1.0 / m1, 0.0))
                distance_from_pole(x, b1 + x * m1))
            |> Array.min

    let max_chroma_for_LH L H =
        let hrad = radians H
        get_bounds L 
            |> Array.map(fun line -> length_of_ray_until_intersect (hrad, line)) 
            |> Array.filter(fun item -> item.IsSome) |> Array.map(fun item -> item.Value) // feels hacky here
            |> Array.min

    let dot_product a b =
        Array.map2 (fun x y -> x * y) a b |> Array.sum

    let f t =
        if t > epsilon then
            116.0 * Math.Pow((t / refY), 1.0 / 3.0) - 16.0
        else
            (t / refY) * kappa

    let f_inv t =
        if t > 8.0 then
            refY * Math.Pow((t + 16.0) / 116.0, 3.0)
        else
            refY * t / kappa

    let from_linear c =
        if c <= 0.0031308 then
            12.92 * c
        else
            1.055 * Math.Pow(c, 1.0 / 2.4) - 0.055

    let to_linear c =
        if c > 0.04045 then
            Math.Pow((c + 0.055) / (1.0 + 0.055), 2.4)
        else
            (c / 12.92)

    // this method took a bit of work to figure out its intent to return a triple
    let rgb_prepare triple =
        let validate (ch:float) =
            let ch2 = Math.Round(ch, 3)
            if ch2 < -0.0001 || ch2 > 1.0001 then
                failwithf "Illegal RGB value %f" ch
            ch
        let normalize ch =
            if ch < 0.0 then
                0.0
            else if ch > 1.0 then
                255.0
            else
                ch * 255.0
        let prep (ch:float) =
            int(Math.Round(ch |> validate |> normalize, 0))

        let r, g, b = triple
        (prep r, prep g, prep b)

    let hex_to_rgb hex =
        let remove_leading_slash (str:string) =
            if str.StartsWith("#") then
                str.[1..]
            else
                str
        let hex2 = remove_leading_slash hex
        // had a proble with wrong indicies here
        let r = float(Convert.ToByte(hex2.[0..1], 16)) / 255.0
        let g = float(Convert.ToByte(hex2.[2..3], 16)) / 255.0
        let b = float(Convert.ToByte(hex2.[4..5], 16)) / 255.0
        (r, g, b)

    let rgb_to_hex triple =
        let r, g, b = rgb_prepare triple
        sprintf "#%2X%2X%2X" r g b

    let trip_to_array triple =
        let a,b,c = triple
        [|a;b;c|]

    let xyz_to_rgb triple =
        let res = m |> Array.map (fun ms -> dot_product (ms) (trip_to_array triple)) |> Array.map from_linear 
        (res.[0], res.[1], res.[2])

    let rgb_to_xyz triple = 
        let rgbl = trip_to_array triple |> Array.map to_linear 
        let res = m_inv |> Array.map (fun ms -> dot_product (ms) rgbl)
        (res.[0], res.[1], res.[2])

    let xyz_to_luv triple =
        let X, Y, Z = triple

        if X = 0.0 && Y = 0.0 && Z = 0.0 then
            (0.0, 0.0, 0.0)
        else
            let varU = (4.0 * X) / (X + (15.0 * Y) + (3.0 * Z))
            let varV = (9.0 * Y) / (X + (15.0 * Y) + (3.0 * Z))
            let L = f(Y)

            if L = 0.0 then
                (0.0, 0.0, 0.0)
            else
                let U = 13.0 * L * (varU - refU)
                let V = 13.0 * L * (varV - refV)
                (L, U, V)


    let luv_to_xyz triple =
        let L, U, V = triple

        if L = 0.0 then
            (0.0, 0.0, 0.0)
        else
            let varY = f_inv(L)
            let varU = U / (13.0 * L) + refU
            let varV = V / (13.0 * L) + refV
            let Y = varY * refY
            let X = 0.0 - (9.0 * Y * varU) / ((varU - 4.0) * varV - varU * varV)
            let Z = (9.0 * Y - (15.0 * varV * Y) - (varV * X)) / (3.0 * varV)

            (X, Y, Z)

    let luv_to_lch triple =
        let L, U, V = triple

        let C = Math.Pow(Math.Pow(U, 2.0) + Math.Pow(V, 2.0), (1.0 / 2.0))
        let hrad = Math.Atan2(V, U)
        let H = degrees hrad

        if H < 0.0 then
            (L, C, 360.0 + H)
        else
            (L, C, H)

    let lch_to_luv triple =
        let L, C, H = triple

        let Hrad = radians H
        let U = (Math.Cos(Hrad) * C)
        let V = (Math.Sin(Hrad) * C)

        (L, U, V)

    let husl_to_lch(triple)=
        let H, S, L = triple

        if L > 99.9999999 then
            (100.0, 0.0, H)
        else if L < 0.00000001 then
            (0.0, 0.0, H)
        else
            let mx = max_chroma_for_LH L H
            let C = mx / 100.0 * S
            (L, C, H)

    let lch_to_husl triple =
        let L, C, H = triple

        if L > 99.9999999 then
            (H, 0.0, 100.0)
        else if L < 0.00000001 then
            (H, 0.0, 0.0)
        else 
            let mx = max_chroma_for_LH L H
            let S = C / mx * 100.0
            (H, S, L)

    let huslp_to_lch triple =
        let H, S, L = triple

        if L > 99.9999999 then
            (100.0, 0.0, H)
        else if L < 0.00000001 then
            (0.0, 0.0, H)
        else
            let mx = max_safe_chroma_for_L L
            let C = mx / 100.0 * S
            (L, C, H)

    let lch_to_huslp triple =
        let L, C, H = triple

        if L > 99.9999999 then
            (H, 0.0, 100.0)
        else if L < 0.00000001 then
            (H, 0.0, 0.0)
        else 
            let mx = max_safe_chroma_for_L L
            let S = C / mx * 100.0
            (H, S, L)

    // public interface
    let lch_to_rgb (l, c, h) =
        xyz_to_rgb(luv_to_xyz(lch_to_luv(l, c, h)))

    let rgb_to_lch (r, g, b) =
        luv_to_lch(xyz_to_luv(rgb_to_xyz(r, g, b)))

    let husl_to_rgb (h, s, l) =
        lch_to_rgb(husl_to_lch(h, s, l))

    let husl_to_hex (h, s, l) =
        rgb_to_hex(husl_to_rgb(h, s, l))

    let rgb_to_husl (r, g, b) =
        lch_to_husl(rgb_to_lch(r, g, b))

    let hex_to_husl hex =
        rgb_to_husl(hex_to_rgb(hex))

    let huslp_to_rgb (h, s, l) =
        lch_to_rgb(huslp_to_lch(h, s, l))

    let huslp_to_hex (h, s, l) =
        rgb_to_hex(huslp_to_rgb(h, s, l))

    let rgb_to_huslp (r, g, b)=
        lch_to_huslp(rgb_to_lch(r, g, b))

    let hex_to_huslp hex =
        rgb_to_huslp(hex_to_rgb(hex))

