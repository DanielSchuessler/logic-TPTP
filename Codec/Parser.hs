{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS -cpp #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Parser where
    
import Data.Char
import Text.Printf
import Control.Monad
import Data.List
import Lexer
#if __GLASGOW_HASKELL__ >= 503
import qualified Data.Array as Happy_Data_Array
#else
import qualified Array as Happy_Data_Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import qualified System.IO as Happy_System_IO
import qualified System.IO.Unsafe as Happy_System_IO_Unsafe
import qualified Debug.Trace as Happy_Debug_Trace
#else
import qualified IO as Happy_System_IO
import qualified IOExts as Happy_System_IO_Unsafe
import qualified IOExts as Happy_Debug_Trace
#endif

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn t4 t7 t8 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t52 t53 t54 t55 t56 t57 t59 t60 t61 t64 t65 t67 t68 t69 t70 t72 t73 t74 t75 t76 t77
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 (TPTP_Input)
	| HappyAbsSyn6 (AFormula)
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 ((SourceInfo,UsefulInfo))
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn45 t45
	| HappyAbsSyn46 t46
	| HappyAbsSyn47 t47
	| HappyAbsSyn48 t48
	| HappyAbsSyn49 t49
	| HappyAbsSyn50 t50
	| HappyAbsSyn51 t51
	| HappyAbsSyn52 t52
	| HappyAbsSyn53 t53
	| HappyAbsSyn54 t54
	| HappyAbsSyn55 t55
	| HappyAbsSyn56 t56
	| HappyAbsSyn57 t57
	| HappyAbsSyn58 (UsefulInfo)
	| HappyAbsSyn59 t59
	| HappyAbsSyn60 t60
	| HappyAbsSyn61 t61
	| HappyAbsSyn62 ([GeneralData])
	| HappyAbsSyn63 (GeneralData)
	| HappyAbsSyn64 t64
	| HappyAbsSyn65 t65
	| HappyAbsSyn66 ([[GeneralData]])
	| HappyAbsSyn67 t67
	| HappyAbsSyn68 t68
	| HappyAbsSyn69 t69
	| HappyAbsSyn70 t70
	| HappyAbsSyn71 (Double)
	| HappyAbsSyn72 t72
	| HappyAbsSyn73 t73
	| HappyAbsSyn74 t74
	| HappyAbsSyn75 t75
	| HappyAbsSyn76 t76
	| HappyAbsSyn77 t77

happyActOffsets :: Happy_Data_Array.Array Int Int
happyActOffsets = Happy_Data_Array.listArray (0,204) ([146,146,146,0,0,0,0,0,416,415,414,361,387,40,40,0,391,0,0,0,0,390,383,0,378,0,374,344,344,370,0,369,40,367,0,368,366,829,814,347,0,0,0,0,0,632,0,0,0,340,814,0,0,0,0,0,0,352,0,333,0,334,0,0,0,288,0,326,243,0,306,0,0,0,0,0,814,0,0,0,0,0,0,0,0,0,0,293,0,268,0,0,571,252,40,0,0,0,245,251,223,0,571,240,0,297,238,252,252,252,252,0,252,0,0,0,203,814,0,0,0,0,0,0,814,814,215,202,178,176,0,191,184,0,0,0,0,0,158,157,155,153,0,0,147,0,140,0,134,0,0,101,125,0,0,0,0,794,297,297,0,0,95,0,0,0,252,63,81,52,0,777,60,0,777,0,0,0,0,0,777,0,0,0,0,105,0,84,68,66,64,22,21,18,252,0,0,0,297,55,0,0,0,0,0
	])

happyGotoOffsets :: Happy_Data_Array.Array Int Int
happyGotoOffsets = Happy_Data_Array.listArray (0,204) ([1,9,5,0,0,0,0,0,0,0,0,0,-40,152,-30,0,0,0,0,0,0,0,-29,0,0,0,0,51,25,0,0,0,-2,0,0,0,0,592,228,12,0,0,0,0,0,29,0,0,0,0,543,0,0,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,183,0,0,0,0,0,0,0,0,0,0,11,0,-3,0,0,640,727,-28,0,0,0,78,0,-7,0,686,0,0,1033,0,926,894,862,990,0,958,0,0,0,13,498,0,0,0,0,0,0,453,408,0,0,10,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-27,0,0,0,0,0,0,0,0,0,0,0,0,77,1022,1044,0,0,-22,0,0,0,830,8,0,6,0,363,3,0,318,0,0,0,0,0,273,0,0,0,0,1011,0,0,0,0,0,0,0,138,727,0,0,0,1000,0,0,0,0,0,0
	])

happyDefActions :: Happy_Data_Array.Array Int Int
happyDefActions = Happy_Data_Array.listArray (0,204) ([-121,0,-121,-3,-5,-6,-4,-2,0,0,0,0,0,0,0,-122,0,-110,-113,-112,-111,0,-120,-119,0,-92,0,0,0,0,-11,0,0,0,-90,0,-93,0,0,-120,-12,-14,-15,-17,-18,-13,-23,-24,-31,0,0,-25,-49,-50,-53,-54,-51,0,-61,-52,-66,-68,-64,-70,-71,-55,-75,-77,-60,-79,-81,-62,-69,-78,-82,-72,0,-40,-41,-48,-73,-114,-115,-83,-117,-118,-116,-120,-33,-125,-38,-36,0,0,0,-91,-94,-37,0,0,-125,-34,0,0,-10,-97,0,0,0,0,0,-57,0,-58,-59,-30,0,0,-42,-43,-45,-46,-47,-44,0,0,0,0,-127,-123,-16,0,-28,-39,-63,-74,-65,-56,-84,0,0,0,-26,-100,-120,-86,-95,-103,-98,-101,-102,0,0,-35,-126,-32,-8,0,-97,-97,-9,-88,0,-80,-76,-67,0,0,0,-123,-21,0,-127,-19,0,-7,-20,-128,-22,-124,0,-29,-85,-87,-89,-97,-96,-108,0,0,0,-31,-25,0,-48,-105,-104,-99,-97,0,-106,-27,-107,-109
	])

happyCheck :: Happy_Data_Array.Array Int Int
happyCheck = Happy_Data_Array.listArray (0,1118) ([-1,0,1,2,3,4,1,2,3,4,1,2,3,4,21,12,5,5,21,1,14,14,12,2,2,17,53,56,68,57,17,6,54,63,64,63,64,19,20,61,69,23,69,22,22,27,28,29,30,31,32,33,34,35,25,57,55,6,50,4,55,63,64,50,55,72,2,27,2,72,2,70,32,21,34,70,73,71,71,70,69,69,22,73,7,8,9,10,11,5,13,10,15,16,31,18,19,20,3,22,23,24,1,26,27,28,29,30,31,4,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,32,33,34,6,27,28,29,1,31,32,33,34,35,64,65,66,67,7,8,9,10,11,10,13,5,15,16,2,18,2,20,2,22,23,24,5,26,27,28,29,30,31,24,25,26,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,5,7,8,9,10,11,4,13,21,15,16,22,18,64,65,66,67,23,24,6,26,27,28,29,30,31,63,64,2,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,31,7,8,9,10,11,2,13,2,15,16,22,18,64,65,66,67,23,24,2,26,27,28,29,30,31,17,18,17,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,27,28,29,30,31,32,33,34,35,15,16,22,18,64,65,66,67,23,24,5,26,27,28,29,30,31,17,18,1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,27,28,29,1,31,32,33,34,35,15,16,1,18,64,65,66,67,23,24,3,26,27,28,29,30,31,17,18,5,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,17,18,5,4,6,5,5,32,3,15,16,2,18,64,65,66,67,23,24,5,26,27,28,29,30,31,5,5,36,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,27,1,1,1,-1,-1,-1,-1,-1,15,16,-1,18,64,65,66,67,23,24,-1,26,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,16,-1,18,64,65,66,67,23,24,-1,26,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,16,-1,18,64,65,66,67,23,24,-1,26,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,16,-1,18,64,65,66,67,23,24,-1,26,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,23,-1,-1,-1,27,28,29,30,31,32,33,34,35,64,65,66,67,19,20,-1,22,23,-1,-1,-1,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,11,12,13,14,15,16,-1,-1,-1,-1,21,22,-1,64,65,66,67,20,-1,22,23,-1,-1,-1,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,22,23,-1,-1,-1,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,27,28,29,30,31,-1,-1,-1,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,1,19,20,-1,-1,23,-1,-1,-1,27,28,29,30,31,32,33,34,35,19,20,1,-1,23,-1,-1,-1,27,28,29,30,31,32,33,34,35,1,-1,-1,19,20,-1,-1,23,-1,-1,-1,27,28,29,30,31,32,33,34,35,-1,-1,23,-1,-1,-1,27,28,29,30,31,32,33,34,35,-1,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,65,66,67,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,50,-1,-1,-1,64,65,66,67,58,59,60,50,62,-1,64,-1,-1,67,-1,58,59,60,50,62,-1,64,-1,-1,67,-1,58,59,60,50,62,52,64,-1,-1,67,-1,58,59,60,50,-1,-1,64,-1,-1,67,-1,58,59,60,-1,-1,-1,64,-1,-1,67,-1,-1,-1,-1,-1,-1,-1
	])

happyTable :: Happy_Data_Array.Array Int Int
happyTable = Happy_Data_Array.listArray (0,1118) ([0,11,2,3,4,5,2,3,4,5,2,3,4,5,100,172,103,126,100,77,169,169,172,-36,-38,181,160,24,22,96,131,29,183,16,17,36,17,78,79,184,25,195,161,-36,-38,19,81,82,83,84,20,85,86,87,117,35,6,31,132,203,6,36,17,132,6,154,196,19,197,101,198,7,20,172,21,15,177,179,170,7,104,104,175,173,189,40,41,42,43,199,44,181,45,46,84,47,190,88,186,89,191,49,158,50,192,52,53,54,55,201,110,111,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,110,111,112,157,19,151,152,159,84,20,85,86,87,72,73,74,75,106,40,41,42,43,160,44,163,45,46,164,47,165,99,166,89,191,49,167,50,192,52,53,54,55,9,10,11,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,168,106,40,41,42,43,169,44,172,45,46,175,47,72,73,74,75,48,49,176,50,51,52,53,54,55,21,17,128,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,84,39,40,41,42,43,143,44,153,45,46,103,47,72,73,74,75,48,49,156,50,51,52,53,54,55,-65,-65,114,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,19,81,82,83,84,20,85,86,87,201,46,103,47,72,73,74,75,48,49,106,50,51,52,53,54,55,-74,-74,108,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,19,151,152,109,84,20,85,86,87,176,46,110,47,72,73,74,75,48,49,117,50,51,52,53,54,55,-63,-63,106,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,114,115,95,96,35,38,39,31,33,178,46,34,47,72,73,74,75,48,49,27,50,51,52,53,54,55,28,29,-1,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,24,13,14,15,0,0,0,0,0,128,46,0,47,72,73,74,75,48,49,0,50,51,52,53,54,55,0,0,0,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,0,0,0,0,0,0,0,0,0,129,46,0,47,72,73,74,75,48,49,0,50,51,52,53,54,55,0,0,0,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,0,0,0,0,0,0,0,0,0,130,46,0,47,72,73,74,75,48,49,0,50,51,52,53,54,55,0,0,0,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,0,0,0,0,0,0,0,0,0,115,46,0,47,72,73,74,75,48,49,0,50,51,52,53,54,55,0,0,0,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,94,0,0,0,19,81,82,83,84,20,85,86,87,72,73,74,75,87,88,0,89,90,0,0,0,91,52,53,54,55,0,0,0,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,119,120,121,122,123,124,0,0,0,0,125,126,0,72,73,74,75,99,0,89,90,0,0,0,91,52,53,54,55,0,0,0,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,0,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,153,90,0,0,0,91,52,53,54,55,0,0,0,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,0,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,97,52,53,54,55,0,0,0,56,98,58,59,60,61,62,63,64,65,66,67,68,69,70,71,77,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,194,78,79,0,0,80,0,0,0,19,81,82,83,84,20,85,86,87,78,79,77,0,195,0,0,0,19,81,82,83,84,20,85,86,87,93,0,0,78,79,0,0,80,0,0,0,19,81,82,83,84,20,85,86,87,0,0,94,0,0,0,19,81,82,83,84,20,85,86,87,0,138,58,134,60,61,62,63,64,135,66,67,136,69,70,71,182,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,138,58,134,60,61,62,63,64,135,66,67,136,69,70,71,139,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,138,58,134,60,61,62,63,64,135,66,67,136,69,70,71,140,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,138,58,134,60,61,62,63,64,135,66,67,136,69,70,71,141,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,133,58,134,60,61,62,63,64,135,66,67,136,69,70,71,0,0,0,0,0,0,0,0,0,0,0,0,0,72,73,74,75,137,58,134,60,61,62,63,64,135,66,67,136,69,70,71,0,0,0,0,0,0,0,0,0,143,0,0,0,72,73,74,75,187,146,147,143,203,0,148,0,0,149,0,187,146,147,143,199,0,148,0,0,149,0,187,146,147,143,188,144,148,0,0,149,0,145,146,147,143,0,0,148,0,0,149,0,186,146,147,0,0,0,148,0,0,149,0,0,0,0,0,0,0
	])

happyReduceArr = Happy_Data_Array.array (1, 127) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127)
	]

happy_n_terms = 37 :: Int
happy_n_nonterms = 74 :: Int

happyReduce_1 = happySpecReduce_1  0 happyReduction_1
happyReduction_1 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  1 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  1 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn5
		 (error "support for 'include' not implemented"
	)

happyReduce_4 = happySpecReduce_1  2 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  2 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 10 3 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (AFormula          happy_var_3               happy_var_5                  %7      (fst happy_var_8) (snd happy_var_8)
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 10 4 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AFormula          happy_var_3         (cnf_to_fof happy_var_5)           %7      (fst happy_var_8) (snd happy_var_8)
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn56  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 ((happy_var_1,happy_var_2)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 ((NoSourceInfo, NoUsefulInfo)
	)

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyTerminal (LowerWord happy_var_1))
	 =  HappyAbsSyn10
		 (Role happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (foldl1 (.|.) (happy_var_1 .|. happy_var_2) happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  12 happyReduction_19
happyReduction_19 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 13 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (foldl1 (:&:) (happy_var_1 :&: happy_var_2) happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 6 16 happyReduction_26
happyReduction_26 ((HappyAbsSyn19  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (happy_var_1                happy_var_3                           happy_var_6
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  18 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  19 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  20 happyReduction_33
happyReduction_33 (HappyAbsSyn76  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn24
		 (foldl1 (:|:) happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  21 happyReduction_34
happyReduction_34 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  22 happyReduction_36
happyReduction_36 (HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (:~: happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  22 happyReduction_37
happyReduction_37 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  23 happyReduction_38
happyReduction_38 (HappyAbsSyn40  happy_var_3)
	(HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn28
		 (all
	)

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn28
		 (exists
	)

happyReduce_41 = happySpecReduce_1  25 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn29
		 ((.<=>.)
	)

happyReduce_42 = happySpecReduce_1  25 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn29
		 ((.=>.)
	)

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn29
		 ((.<=.)
	)

happyReduce_44 = happySpecReduce_1  25 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn29
		 ((.<~>.)
	)

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn29
		 ((.~|.)
	)

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn29
		 ((.~&.)
	)

happyReduce_47 = happySpecReduce_1  26 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn30
		 (.~.
	)

happyReduce_48 = happySpecReduce_1  27 happyReduction_48
happyReduction_48 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  27 happyReduction_49
happyReduction_49 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  27 happyReduction_50
happyReduction_50 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  28 happyReduction_51
happyReduction_51 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  29 happyReduction_52
happyReduction_52 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  29 happyReduction_53
happyReduction_53 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  30 happyReduction_54
happyReduction_54 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  31 happyReduction_55
happyReduction_55 (HappyAbsSyn40  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  32 happyReduction_56
happyReduction_56 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  33 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn37
		 ((.=.)
	)

happyReduce_58 = happySpecReduce_1  34 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn38
		 ((.!=.)
	)

happyReduce_59 = happySpecReduce_1  35 happyReduction_59
happyReduction_59 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  36 happyReduction_60
happyReduction_60 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  36 happyReduction_61
happyReduction_61 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  37 happyReduction_62
happyReduction_62 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  37 happyReduction_63
happyReduction_63 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  37 happyReduction_64
happyReduction_64 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  38 happyReduction_65
happyReduction_65 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happyReduce 4 38 happyReduction_66
happyReduction_66 (_ `HappyStk`
	(HappyAbsSyn55  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn42
		 (happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_1  39 happyReduction_67
happyReduction_67 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  40 happyReduction_68
happyReduction_68 (HappyAbsSyn68  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  41 happyReduction_69
happyReduction_69 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  41 happyReduction_70
happyReduction_70 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  42 happyReduction_71
happyReduction_71 (HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn46
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  42 happyReduction_72
happyReduction_72 (HappyTerminal (DoubleQuoted happy_var_1))
	 =  HappyAbsSyn46
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  43 happyReduction_73
happyReduction_73 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  44 happyReduction_74
happyReduction_74 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happyReduce 4 44 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyAbsSyn55  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_1  45 happyReduction_76
happyReduction_76 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  46 happyReduction_77
happyReduction_77 (HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  47 happyReduction_78
happyReduction_78 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happyReduce 4 47 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn55  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn51
		 (happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_80 = happySpecReduce_1  48 happyReduction_80
happyReduction_80 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn52
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  49 happyReduction_81
happyReduction_81 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  50 happyReduction_82
happyReduction_82 (HappyTerminal (UpperWord happy_var_1))
	 =  HappyAbsSyn54
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  51 happyReduction_83
happyReduction_83 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn55
		 ([happy_var_1]
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  51 happyReduction_84
happyReduction_84 _
	(HappyTerminal happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 : happy_var_2
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  52 happyReduction_85
happyReduction_85 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_2  53 happyReduction_86
happyReduction_86 (HappyAbsSyn58  happy_var_2)
	_
	 =  HappyAbsSyn57
		 (happy_var_2
	)
happyReduction_86 _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  53 happyReduction_87
happyReduction_87 _
	 =  HappyAbsSyn57
		 (NoUsefulInfo
	)

happyReduce_88 = happySpecReduce_1  54 happyReduction_88
happyReduction_88 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happyReduce 6 55 happyReduction_89
happyReduction_89 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn59
		 (error "Sorry, 'include' not implemented"
	) `HappyStk` happyRest

happyReduce_90 = happyReduce 4 56 happyReduction_90
happyReduction_90 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (undefined
	) `HappyStk` happyRest

happyReduce_91 = happySpecReduce_1  56 happyReduction_91
happyReduction_91 _
	 =  HappyAbsSyn60
		 (undefined
	)

happyReduce_92 = happySpecReduce_1  57 happyReduction_92
happyReduction_92 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn61
		 ([happy_var_1]
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  57 happyReduction_93
happyReduction_93 _
	(HappyTerminal happy_var_2)
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn61
		 (happy_var_1 : happy_var_2
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  58 happyReduction_94
happyReduction_94 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn62
		 ([happy_var_1]
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  58 happyReduction_95
happyReduction_95 (HappyAbsSyn62  happy_var_3)
	_
	(HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1 : happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_0  58 happyReduction_96
happyReduction_96  =  HappyAbsSyn62
		 ([]
	)

happyReduce_97 = happySpecReduce_1  59 happyReduction_97
happyReduction_97 (HappyAbsSyn68  happy_var_1)
	 =  HappyAbsSyn63
		 (GWord happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happyReduce 4 59 happyReduction_98
happyReduction_98 (_ `HappyStk`
	(HappyAbsSyn66  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn68  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn63
		 (GApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_99 = happySpecReduce_1  59 happyReduction_99
happyReduction_99 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn63
		 (GVar happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  59 happyReduction_100
happyReduction_100 (HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn63
		 (GNumber happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  59 happyReduction_101
happyReduction_101 (HappyTerminal (DoubleQuoted happy_var_1))
	 =  HappyAbsSyn63
		 (GDistinctObject happy_var_1
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  59 happyReduction_102
happyReduction_102 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn63
		 (happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happyReduce 4 60 happyReduction_103
happyReduction_103 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn64
		 (error "formula_data not implemented"
	) `HappyStk` happyRest

happyReduce_104 = happyReduce 4 60 happyReduction_104
happyReduction_104 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn64
		 (error "formula_data not implemented"
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_2  61 happyReduction_105
happyReduction_105 _
	_
	 =  HappyAbsSyn65
		 ([]
	)

happyReduce_106 = happySpecReduce_3  61 happyReduction_106
happyReduction_106 _
	(HappyAbsSyn66  happy_var_2)
	_
	 =  HappyAbsSyn65
		 (happy_var_2
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  62 happyReduction_107
happyReduction_107 _
	 =  HappyAbsSyn66
		 ([]
	)

happyReduce_108 = happySpecReduce_3  62 happyReduction_108
happyReduction_108 (HappyAbsSyn66  happy_var_3)
	_
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn66
		 (happy_var_1 : happy_var_3
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  63 happyReduction_109
happyReduction_109 (HappyAbsSyn68  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  63 happyReduction_110
happyReduction_110 (HappyTerminal (UnsignedInt happy_var_1))
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  64 happyReduction_111
happyReduction_111 (HappyTerminal (LowerWord happy_var_1))
	 =  HappyAbsSyn68
		 (happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  64 happyReduction_112
happyReduction_112 (HappyTerminal (SingleQuoted happy_var_1))
	 =  HappyAbsSyn68
		 (happy_var_1
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  65 happyReduction_113
happyReduction_113 (HappyTerminal (DollarWord happy_var_1))
	 =  HappyAbsSyn69
		 (happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  66 happyReduction_114
happyReduction_114 (HappyTerminal (DollarDollarWord happy_var_1))
	 =  HappyAbsSyn70
		 (happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  67 happyReduction_115
happyReduction_115 (HappyTerminal (Real happy_var_1))
	 =  HappyAbsSyn71
		 (happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  67 happyReduction_116
happyReduction_116 (HappyTerminal (SignedInt happy_var_1))
	 =  HappyAbsSyn71
		 (fromIntegral happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  67 happyReduction_117
happyReduction_117 (HappyTerminal (UnsignedInt happy_var_1))
	 =  HappyAbsSyn71
		 (fromIntegral happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  68 happyReduction_118
happyReduction_118 (HappyTerminal (SingleQuoted happy_var_1))
	 =  HappyAbsSyn72
		 (happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_0  69 happyReduction_119
happyReduction_119  =  HappyAbsSyn73
		 (()
	)

happyReduce_120 = happySpecReduce_0  70 happyReduction_120
happyReduction_120  =  HappyAbsSyn74
		 ([]
	)

happyReduce_121 = happySpecReduce_2  70 happyReduction_121
happyReduction_121 (HappyAbsSyn74  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn74
		 (happy_var_1 : happy_var_2
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_0  71 happyReduction_122
happyReduction_122  =  HappyAbsSyn75
		 ([]
	)

happyReduce_123 = happySpecReduce_2  71 happyReduction_123
happyReduction_123 (HappyAbsSyn75  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn75
		 (happy_var_1 : happy_var_2
	)
happyReduction_123 _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_0  72 happyReduction_124
happyReduction_124  =  HappyAbsSyn76
		 ([]
	)

happyReduce_125 = happySpecReduce_2  72 happyReduction_125
happyReduction_125 (HappyAbsSyn76  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn76
		 (happy_var_1 : happy_var_2
	)
happyReduction_125 _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_0  73 happyReduction_126
happyReduction_126  =  HappyAbsSyn77
		 ([]
	)

happyReduce_127 = happySpecReduce_2  73 happyReduction_127
happyReduction_127 (HappyAbsSyn77  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn77
		 (happy_var_1 : happy_var_2
	)
happyReduction_127 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	happyDoAction 36 notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	LP -> cont 1;
	RP -> cont 2;
	Lbrack -> cont 3;
	Rbrack -> cont 4;
	Comma -> cont 5;
	Dot -> cont 6;
	Star -> cont 7;
	Plus -> cont 8;
	Rangle -> cont 9;
	Op ":" -> cont 10;
	Op "<=>" -> cont 11;
	Op "=>" -> cont 12;
	Op "<~>" -> cont 13;
	Op "~|" -> cont 14;
	Op "~&" -> cont 15;
	Op "<=" -> cont 16;
	Op "=" -> cont 17;
	Op "!=" -> cont 18;
	Op "!" -> cont 19;
	Op "?" -> cont 20;
	Op "&" -> cont 21;
	Op "|" -> cont 22;
	Op "~" -> cont 23;
	LowerWord "fof" -> cont 24;
	LowerWord "cnf" -> cont 25;
	LowerWord "include" -> cont 26;
	SingleQuoted happy_dollar_dollar -> cont 27;
	DoubleQuoted happy_dollar_dollar -> cont 28;
	DollarWord happy_dollar_dollar -> cont 29;
	DollarDollarWord happy_dollar_dollar -> cont 30;
	UpperWord happy_dollar_dollar -> cont 31;
	LowerWord happy_dollar_dollar -> cont 32;
	SignedInt happy_dollar_dollar -> cont 33;
	UnsignedInt happy_dollar_dollar -> cont 34;
	Real happy_dollar_dollar -> cont 35;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . ((\xs -> case xs of
                    xs -> error ("Parse error, pos: "++show (take 10 xs))))

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data TPTP_Input = 
    -- | Annotated formula
    AFormula {
      name :: String 
    , role :: Role 
    , formula :: Formula 
    , sourceInfo :: SourceInfo 
    , usefulInfo :: UsefulInfo
    }
          deriving (Eq,Ord,Show,Read,Data,Typeable)
                   
data SourceInfo = NoSourceInfo
                  deriving (Eq,Ord,Show,Read,Data,Typeable)
              
data UsefulInfo = NoUsefulInfo
                deriving (Eq,Ord,Show,Read,Data,Typeable)
                           
data Role = Role String
          deriving (Eq,Ord,Show,Read,Data,Typeable)

                   
-- | Let's make this extensible by leaving the recursion explicit
data Formula0 term formula = 
              formula :<=>: formula -- ^ Equivalence
            | formula :=>: formula -- ^ Implication
            | formula :<=: formula -- ^ Implication (reverse)
            | formula :&: formula -- ^ AND
            | formula :|: formula -- ^ OR
            | formula :~&: formula -- ^ NAND
            | formula :~|: formula -- ^ NOR
            | formula :<~>: formula -- ^ XOR
            | term :=: term -- ^ Equality
            | term :!=: term -- ^ Inequality
            | PApp String [term] -- ^ Predicate application
            | Exists [String] formula -- ^ Ex. quantification
            | All [String] formula -- ^ Univ. quantification
            | :~: formula -- ^ Negation
          deriving (Eq,Ord,Show,Read,Data,Typeable)
                   
infixl 2 :<=>: .<=>. :=>: .=>. :<=: .<=.
infixl 3 :|: .|. :~|: .~|.
infixl 4 :&: .&. :~&: .~&.
infixl 5 :=: .=. :!=: .!=.
                   
                       
-- | See 'Formula0'
data Term0 term =
          | Var String
          | Constant String
          | FApp String [term] 
          deriving (Eq,Ord,Show,Read,Data,Typeable)


-- | Basic first-order formulae                   
newtype Formula = Formula (Formula0 Term Formula)
          deriving (Eq,Ord,Show,Read,Data,Typeable)

-- | Basic terms
newtype Term = Term (Term0 Term)
          deriving (Eq,Ord,Show,Read,Data,Typeable)

                   
-- * Constructor wrappers
                   
#define FF Formula
#define TT Term

x .<=>. y = FF x :<=>: FF y  
x .=>.  y = FF x :=>:  FF y  
x .<=.  y = FF x :<=:  FF y  
x .~|.  y = FF x :~|:  FF y  
x .|.   y = FF x :|:   FF y  
x .~&.  y = FF x :~&:  FF y  
x .&.   y = FF x :&:   FF y  
(.~.) x = :~: (FF x)
x .=. y   = TT x :=:   TT y 
x .!=. y  = TT x :!=:  TT y 
all vars x = All vars (FF x)
exists vars x = Exists vars (FF x)
                
var x = Var x

-- * Misc

cnf_to_fof = error "Sorry, CNF not implemented yet"

data GeneralData = GWord String
                 | GApp String [[GeneralData]]
                 | GVar String
                 | GNumber Double
                 | GDistinctObject String
                 | GFormulaData 

          deriving (Eq,Ord,Show,Read,Data,Typeable)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int Happy_IntList





{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}



happyTrace string expr = unsafePerformIO $ do
    hPutStr stderr string
    return expr




infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (0), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (0) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= (happyTrace ("state: " ++ show (st) ++  		      ",\ttoken: " ++ show (i) ++ 		      ",\taction: ")) $


	  case action of
		(0)		  -> (happyTrace ("fail.\n")) $
				     happyFail i tk st
		(-1) 	  -> (happyTrace ("accept.\n")) $
				     happyAccept i tk st
		n | (n < ((0) :: Int)) -> (happyTrace ("reduce (rule " ++ show rule 						 ++ ")")) $

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = ((negate ((n + ((1) :: Int)))))
		n		  -> (happyTrace ("shift, enter state " 						 ++ show (new_state) 						 ++ "\n")) $


				     happyShift new_state i tk st
				     where new_state = (n - ((1) :: Int))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off + i)
	 check  = if (off_i >= ((0) :: Int))
			then (indexShortOffAddr happyCheck off_i ==  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off








-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (0) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (0) tk st sts stk
     = happyFail (0) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off + nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop (0) l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   (happyTrace (", goto state " ++ show (new_state) ++ "\n")) $
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off + nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery ((0) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (0) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (0) tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction (0) tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction (0) tk action sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
