namespace Fhusl
module Convert =
    val lch_to_rgb : float * float * float -> float * float * float
    val rgb_to_lch : float * float * float -> float * float * float
    val husl_to_rgb : float * float * float -> float * float * float
    val husl_to_hex : float * float * float -> string
    val rgb_to_husl : float * float * float -> float * float * float
    val hex_to_husl : string -> float * float * float
    val huslp_to_rgb : float * float * float -> float * float * float
    val huslp_to_hex : float * float * float -> string
    val rgb_to_huslp : float * float * float -> float * float * float
    val hex_to_huslp : string -> float * float * float
