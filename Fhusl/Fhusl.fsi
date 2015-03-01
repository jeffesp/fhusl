namespace Fhusl
module Convert =
    val LCHToRGB : float * float * float -> float * float * float
    val RGBToLCH : float * float * float -> float * float * float
    val HUSLToRGB : float * float * float -> float * float * float
    val HUSLToHex : float * float * float -> string
    val RGBToHUSL : float * float * float -> float * float * float
    val HexToHUSL : string -> float * float * float
    val HUSLPToRGB : float * float * float -> float * float * float
    val HUSLPToHex : float * float * float -> string
    val RGBToHUSLP : float * float * float -> float * float * float
    val HexToHUSLP : string -> float * float * float
