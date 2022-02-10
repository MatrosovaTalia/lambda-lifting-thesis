{-# OPTIONS_GHC -w #-}
module Main where
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,501) ([0,57472,65,0,0,0,0,0,0,0,0,0,0,0,0,7688,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8256,0,0,0,0,0,4,0,0,15872,10240,0,0,1,0,0,3968,2560,0,0,0,1,0,992,640,0,0,0,0,0,0,0,0,0,0,0,0,45056,63744,0,0,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,124,80,0,15872,40960,0,0,0,256,0,0,16428,62,0,0,1,0,992,640,0,61440,16385,1,0,0,0,0,0,352,496,0,0,16,0,0,16472,124,32768,0,0,0,1984,1280,0,57344,32771,2,0,496,320,0,2048,3102,0,0,124,80,0,15872,10240,0,0,31,20,0,3968,2560,0,49152,7,5,0,0,128,0,0,0,2,0,0,0,0,31744,16384,1,0,45056,63504,0,7936,5120,0,0,0,1,0,0,512,0,0,0,0,0,15376,24,0,0,704,993,0,0,0,0,0,0,0,0,0,0,0,3968,2560,0,0,5632,0,0,0,11,0,0,1408,0,0,49152,32770,3,0,352,448,0,64,0,0,49408,387,0,0,0,0,0,0,0,0,57344,32771,2,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,64,0,3968,2560,0,0,0,0,0,0,0,0,4096,0,0,0,7688,1036,0,0,352,496,0,0,0,0,0,0,0,0,11264,15904,0,0,0,0,0,132,0,0,0,0,0,2048,3102,0,0,124,80,0,16384,0,0,0,33729,1,0,0,0,0,0,8,0,0,0,0,0,0,34176,1984,0,256,0,0,0,0,0,0,0,256,0,0,0,0,0,0,1,0,61504,96,0,0,4,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_toygrammar","Program","RoutineDeclaration","Parameters","Array","ArrayElement","Primaries","Body","BodyDeclaration","Statement","Return","Assignment","RoutineCall","Expressions","WhileLoop","ForLoop","IfStatement","Expression","Primary","Print","Identifier","IDENTIFIER","INTEGER_LITERAL","REAL_LITERAL","TRUE","FALSE","END","DEF","WHILE","FOR","IF","ELSE","AND","OR","NOT","XOR","PRINT","RETURN","NEWLINE","LPAREN","RPAREN","LBRACKET","RBRACKET","COMMA","COLON","IN_RANGE","EQUALS","PLUS","MINUS","MUL","DIV","REM","%eof"]
        bit_start = st Prelude.* 55
        bit_end = (st Prelude.+ 1) Prelude.* 55
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..54]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (24) = happyShift action_12
action_0 (30) = happyShift action_13
action_0 (31) = happyShift action_14
action_0 (32) = happyShift action_15
action_0 (33) = happyShift action_16
action_0 (39) = happyShift action_17
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (12) = happyGoto action_4
action_0 (14) = happyGoto action_5
action_0 (15) = happyGoto action_6
action_0 (17) = happyGoto action_7
action_0 (18) = happyGoto action_8
action_0 (19) = happyGoto action_9
action_0 (22) = happyGoto action_10
action_0 (23) = happyGoto action_11
action_0 _ = happyReduce_1

action_1 _ = happyFail (happyExpListPerState 1)

action_2 (55) = happyAccept
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_21

action_4 (24) = happyShift action_12
action_4 (30) = happyShift action_13
action_4 (31) = happyShift action_14
action_4 (32) = happyShift action_15
action_4 (33) = happyShift action_16
action_4 (39) = happyShift action_17
action_4 (4) = happyGoto action_36
action_4 (5) = happyGoto action_3
action_4 (12) = happyGoto action_4
action_4 (14) = happyGoto action_5
action_4 (15) = happyGoto action_6
action_4 (17) = happyGoto action_7
action_4 (18) = happyGoto action_8
action_4 (19) = happyGoto action_9
action_4 (22) = happyGoto action_10
action_4 (23) = happyGoto action_11
action_4 _ = happyReduce_1

action_5 _ = happyReduce_15

action_6 _ = happyReduce_16

action_7 _ = happyReduce_17

action_8 _ = happyReduce_18

action_9 _ = happyReduce_19

action_10 _ = happyReduce_20

action_11 (42) = happyShift action_34
action_11 (49) = happyShift action_35
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_49

action_13 (24) = happyShift action_12
action_13 (23) = happyGoto action_33
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (24) = happyShift action_12
action_14 (25) = happyShift action_25
action_14 (26) = happyShift action_26
action_14 (27) = happyShift action_27
action_14 (28) = happyShift action_28
action_14 (42) = happyShift action_29
action_14 (44) = happyShift action_30
action_14 (7) = happyGoto action_19
action_14 (8) = happyGoto action_20
action_14 (15) = happyGoto action_21
action_14 (20) = happyGoto action_32
action_14 (21) = happyGoto action_23
action_14 (23) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (24) = happyShift action_12
action_15 (23) = happyGoto action_31
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (24) = happyShift action_12
action_16 (25) = happyShift action_25
action_16 (26) = happyShift action_26
action_16 (27) = happyShift action_27
action_16 (28) = happyShift action_28
action_16 (42) = happyShift action_29
action_16 (44) = happyShift action_30
action_16 (7) = happyGoto action_19
action_16 (8) = happyGoto action_20
action_16 (15) = happyGoto action_21
action_16 (20) = happyGoto action_22
action_16 (21) = happyGoto action_23
action_16 (23) = happyGoto action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (42) = happyShift action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (24) = happyShift action_12
action_18 (25) = happyShift action_25
action_18 (26) = happyShift action_26
action_18 (27) = happyShift action_27
action_18 (28) = happyShift action_28
action_18 (42) = happyShift action_29
action_18 (44) = happyShift action_30
action_18 (7) = happyGoto action_19
action_18 (8) = happyGoto action_20
action_18 (15) = happyGoto action_21
action_18 (16) = happyGoto action_57
action_18 (20) = happyGoto action_39
action_18 (21) = happyGoto action_23
action_18 (23) = happyGoto action_24
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_47

action_20 _ = happyReduce_46

action_21 _ = happyReduce_45

action_22 (35) = happyShift action_41
action_22 (36) = happyShift action_42
action_22 (38) = happyShift action_43
action_22 (47) = happyShift action_56
action_22 (50) = happyShift action_45
action_22 (51) = happyShift action_46
action_22 (52) = happyShift action_47
action_22 (53) = happyShift action_48
action_22 (54) = happyShift action_49
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_40

action_24 (42) = happyShift action_34
action_24 (44) = happyShift action_55
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_41

action_26 _ = happyReduce_42

action_27 _ = happyReduce_43

action_28 _ = happyReduce_44

action_29 (24) = happyShift action_12
action_29 (25) = happyShift action_25
action_29 (26) = happyShift action_26
action_29 (27) = happyShift action_27
action_29 (28) = happyShift action_28
action_29 (42) = happyShift action_29
action_29 (44) = happyShift action_30
action_29 (7) = happyGoto action_19
action_29 (8) = happyGoto action_20
action_29 (15) = happyGoto action_21
action_29 (20) = happyGoto action_54
action_29 (21) = happyGoto action_23
action_29 (23) = happyGoto action_24
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (24) = happyShift action_12
action_30 (25) = happyShift action_25
action_30 (26) = happyShift action_26
action_30 (27) = happyShift action_27
action_30 (28) = happyShift action_28
action_30 (44) = happyShift action_30
action_30 (46) = happyShift action_53
action_30 (7) = happyGoto action_19
action_30 (8) = happyGoto action_20
action_30 (9) = happyGoto action_51
action_30 (15) = happyGoto action_21
action_30 (21) = happyGoto action_52
action_30 (23) = happyGoto action_24
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (48) = happyShift action_50
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (35) = happyShift action_41
action_32 (36) = happyShift action_42
action_32 (38) = happyShift action_43
action_32 (47) = happyShift action_44
action_32 (50) = happyShift action_45
action_32 (51) = happyShift action_46
action_32 (52) = happyShift action_47
action_32 (53) = happyShift action_48
action_32 (54) = happyShift action_49
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (42) = happyShift action_40
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (24) = happyShift action_12
action_34 (25) = happyShift action_25
action_34 (26) = happyShift action_26
action_34 (27) = happyShift action_27
action_34 (28) = happyShift action_28
action_34 (42) = happyShift action_29
action_34 (44) = happyShift action_30
action_34 (7) = happyGoto action_19
action_34 (8) = happyGoto action_20
action_34 (15) = happyGoto action_21
action_34 (16) = happyGoto action_38
action_34 (20) = happyGoto action_39
action_34 (21) = happyGoto action_23
action_34 (23) = happyGoto action_24
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (24) = happyShift action_12
action_35 (25) = happyShift action_25
action_35 (26) = happyShift action_26
action_35 (27) = happyShift action_27
action_35 (28) = happyShift action_28
action_35 (42) = happyShift action_29
action_35 (44) = happyShift action_30
action_35 (7) = happyGoto action_19
action_35 (8) = happyGoto action_20
action_35 (15) = happyGoto action_21
action_35 (20) = happyGoto action_37
action_35 (21) = happyGoto action_23
action_35 (23) = happyGoto action_24
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_2

action_37 (35) = happyShift action_41
action_37 (36) = happyShift action_42
action_37 (38) = happyShift action_43
action_37 (50) = happyShift action_45
action_37 (51) = happyShift action_46
action_37 (52) = happyShift action_47
action_37 (53) = happyShift action_48
action_37 (54) = happyShift action_49
action_37 _ = happyReduce_23

action_38 (43) = happyShift action_81
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (35) = happyShift action_41
action_39 (36) = happyShift action_42
action_39 (38) = happyShift action_43
action_39 (46) = happyShift action_80
action_39 (50) = happyShift action_45
action_39 (51) = happyShift action_46
action_39 (52) = happyShift action_47
action_39 (53) = happyShift action_48
action_39 (54) = happyShift action_49
action_39 _ = happyReduce_25

action_40 (24) = happyShift action_12
action_40 (6) = happyGoto action_78
action_40 (23) = happyGoto action_79
action_40 _ = happyReduce_5

action_41 (24) = happyShift action_12
action_41 (25) = happyShift action_25
action_41 (26) = happyShift action_26
action_41 (27) = happyShift action_27
action_41 (28) = happyShift action_28
action_41 (42) = happyShift action_29
action_41 (44) = happyShift action_30
action_41 (7) = happyGoto action_19
action_41 (8) = happyGoto action_20
action_41 (15) = happyGoto action_21
action_41 (20) = happyGoto action_77
action_41 (21) = happyGoto action_23
action_41 (23) = happyGoto action_24
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (24) = happyShift action_12
action_42 (25) = happyShift action_25
action_42 (26) = happyShift action_26
action_42 (27) = happyShift action_27
action_42 (28) = happyShift action_28
action_42 (42) = happyShift action_29
action_42 (44) = happyShift action_30
action_42 (7) = happyGoto action_19
action_42 (8) = happyGoto action_20
action_42 (15) = happyGoto action_21
action_42 (20) = happyGoto action_76
action_42 (21) = happyGoto action_23
action_42 (23) = happyGoto action_24
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (24) = happyShift action_12
action_43 (25) = happyShift action_25
action_43 (26) = happyShift action_26
action_43 (27) = happyShift action_27
action_43 (28) = happyShift action_28
action_43 (42) = happyShift action_29
action_43 (44) = happyShift action_30
action_43 (7) = happyGoto action_19
action_43 (8) = happyGoto action_20
action_43 (15) = happyGoto action_21
action_43 (20) = happyGoto action_75
action_43 (21) = happyGoto action_23
action_43 (23) = happyGoto action_24
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (24) = happyShift action_12
action_44 (30) = happyShift action_13
action_44 (31) = happyShift action_14
action_44 (32) = happyShift action_15
action_44 (33) = happyShift action_16
action_44 (39) = happyShift action_17
action_44 (40) = happyShift action_74
action_44 (5) = happyGoto action_3
action_44 (10) = happyGoto action_70
action_44 (11) = happyGoto action_71
action_44 (12) = happyGoto action_72
action_44 (13) = happyGoto action_73
action_44 (14) = happyGoto action_5
action_44 (15) = happyGoto action_6
action_44 (17) = happyGoto action_7
action_44 (18) = happyGoto action_8
action_44 (19) = happyGoto action_9
action_44 (22) = happyGoto action_10
action_44 (23) = happyGoto action_11
action_44 _ = happyReduce_11

action_45 (24) = happyShift action_12
action_45 (25) = happyShift action_25
action_45 (26) = happyShift action_26
action_45 (27) = happyShift action_27
action_45 (28) = happyShift action_28
action_45 (42) = happyShift action_29
action_45 (44) = happyShift action_30
action_45 (7) = happyGoto action_19
action_45 (8) = happyGoto action_20
action_45 (15) = happyGoto action_21
action_45 (20) = happyGoto action_69
action_45 (21) = happyGoto action_23
action_45 (23) = happyGoto action_24
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (24) = happyShift action_12
action_46 (25) = happyShift action_25
action_46 (26) = happyShift action_26
action_46 (27) = happyShift action_27
action_46 (28) = happyShift action_28
action_46 (42) = happyShift action_29
action_46 (44) = happyShift action_30
action_46 (7) = happyGoto action_19
action_46 (8) = happyGoto action_20
action_46 (15) = happyGoto action_21
action_46 (20) = happyGoto action_68
action_46 (21) = happyGoto action_23
action_46 (23) = happyGoto action_24
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (24) = happyShift action_12
action_47 (25) = happyShift action_25
action_47 (26) = happyShift action_26
action_47 (27) = happyShift action_27
action_47 (28) = happyShift action_28
action_47 (42) = happyShift action_29
action_47 (44) = happyShift action_30
action_47 (7) = happyGoto action_19
action_47 (8) = happyGoto action_20
action_47 (15) = happyGoto action_21
action_47 (20) = happyGoto action_67
action_47 (21) = happyGoto action_23
action_47 (23) = happyGoto action_24
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (24) = happyShift action_12
action_48 (25) = happyShift action_25
action_48 (26) = happyShift action_26
action_48 (27) = happyShift action_27
action_48 (28) = happyShift action_28
action_48 (42) = happyShift action_29
action_48 (44) = happyShift action_30
action_48 (7) = happyGoto action_19
action_48 (8) = happyGoto action_20
action_48 (15) = happyGoto action_21
action_48 (20) = happyGoto action_66
action_48 (21) = happyGoto action_23
action_48 (23) = happyGoto action_24
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (24) = happyShift action_12
action_49 (25) = happyShift action_25
action_49 (26) = happyShift action_26
action_49 (27) = happyShift action_27
action_49 (28) = happyShift action_28
action_49 (42) = happyShift action_29
action_49 (44) = happyShift action_30
action_49 (7) = happyGoto action_19
action_49 (8) = happyGoto action_20
action_49 (15) = happyGoto action_21
action_49 (20) = happyGoto action_65
action_49 (21) = happyGoto action_23
action_49 (23) = happyGoto action_24
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (42) = happyShift action_64
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (45) = happyShift action_63
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_9

action_53 (24) = happyShift action_12
action_53 (25) = happyShift action_25
action_53 (26) = happyShift action_26
action_53 (27) = happyShift action_27
action_53 (28) = happyShift action_28
action_53 (44) = happyShift action_30
action_53 (46) = happyShift action_53
action_53 (7) = happyGoto action_19
action_53 (8) = happyGoto action_20
action_53 (9) = happyGoto action_62
action_53 (15) = happyGoto action_21
action_53 (21) = happyGoto action_52
action_53 (23) = happyGoto action_24
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (35) = happyShift action_41
action_54 (36) = happyShift action_42
action_54 (38) = happyShift action_43
action_54 (43) = happyShift action_61
action_54 (50) = happyShift action_45
action_54 (51) = happyShift action_46
action_54 (52) = happyShift action_47
action_54 (53) = happyShift action_48
action_54 (54) = happyShift action_49
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (24) = happyShift action_12
action_55 (25) = happyShift action_25
action_55 (26) = happyShift action_26
action_55 (27) = happyShift action_27
action_55 (28) = happyShift action_28
action_55 (42) = happyShift action_29
action_55 (44) = happyShift action_30
action_55 (7) = happyGoto action_19
action_55 (8) = happyGoto action_20
action_55 (15) = happyGoto action_21
action_55 (20) = happyGoto action_60
action_55 (21) = happyGoto action_23
action_55 (23) = happyGoto action_24
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (41) = happyShift action_59
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (43) = happyShift action_58
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_48

action_59 (24) = happyShift action_12
action_59 (30) = happyShift action_13
action_59 (31) = happyShift action_14
action_59 (32) = happyShift action_15
action_59 (33) = happyShift action_16
action_59 (39) = happyShift action_17
action_59 (40) = happyShift action_74
action_59 (5) = happyGoto action_3
action_59 (10) = happyGoto action_90
action_59 (11) = happyGoto action_71
action_59 (12) = happyGoto action_72
action_59 (13) = happyGoto action_73
action_59 (14) = happyGoto action_5
action_59 (15) = happyGoto action_6
action_59 (17) = happyGoto action_7
action_59 (18) = happyGoto action_8
action_59 (19) = happyGoto action_9
action_59 (22) = happyGoto action_10
action_59 (23) = happyGoto action_11
action_59 _ = happyReduce_11

action_60 (35) = happyShift action_41
action_60 (36) = happyShift action_42
action_60 (38) = happyShift action_43
action_60 (45) = happyShift action_89
action_60 (50) = happyShift action_45
action_60 (51) = happyShift action_46
action_60 (52) = happyShift action_47
action_60 (53) = happyShift action_48
action_60 (54) = happyShift action_49
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_39

action_62 _ = happyReduce_10

action_63 _ = happyReduce_7

action_64 (24) = happyShift action_12
action_64 (25) = happyShift action_25
action_64 (26) = happyShift action_26
action_64 (27) = happyShift action_27
action_64 (28) = happyShift action_28
action_64 (42) = happyShift action_29
action_64 (44) = happyShift action_30
action_64 (7) = happyGoto action_19
action_64 (8) = happyGoto action_20
action_64 (15) = happyGoto action_21
action_64 (20) = happyGoto action_88
action_64 (21) = happyGoto action_23
action_64 (23) = happyGoto action_24
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (35) = happyShift action_41
action_65 (36) = happyShift action_42
action_65 (38) = happyShift action_43
action_65 _ = happyReduce_35

action_66 (35) = happyShift action_41
action_66 (36) = happyShift action_42
action_66 (38) = happyShift action_43
action_66 _ = happyReduce_34

action_67 (35) = happyShift action_41
action_67 (36) = happyShift action_42
action_67 (38) = happyShift action_43
action_67 _ = happyReduce_33

action_68 (35) = happyShift action_41
action_68 (36) = happyShift action_42
action_68 (38) = happyShift action_43
action_68 (52) = happyShift action_47
action_68 (53) = happyShift action_48
action_68 (54) = happyShift action_49
action_68 _ = happyReduce_32

action_69 (35) = happyShift action_41
action_69 (36) = happyShift action_42
action_69 (38) = happyShift action_43
action_69 (52) = happyShift action_47
action_69 (53) = happyShift action_48
action_69 (54) = happyShift action_49
action_69 _ = happyReduce_31

action_70 (29) = happyShift action_87
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (24) = happyShift action_12
action_71 (30) = happyShift action_13
action_71 (31) = happyShift action_14
action_71 (32) = happyShift action_15
action_71 (33) = happyShift action_16
action_71 (39) = happyShift action_17
action_71 (40) = happyShift action_74
action_71 (5) = happyGoto action_3
action_71 (10) = happyGoto action_86
action_71 (11) = happyGoto action_71
action_71 (12) = happyGoto action_72
action_71 (13) = happyGoto action_73
action_71 (14) = happyGoto action_5
action_71 (15) = happyGoto action_6
action_71 (17) = happyGoto action_7
action_71 (18) = happyGoto action_8
action_71 (19) = happyGoto action_9
action_71 (22) = happyGoto action_10
action_71 (23) = happyGoto action_11
action_71 _ = happyReduce_11

action_72 _ = happyReduce_14

action_73 _ = happyReduce_13

action_74 (24) = happyShift action_12
action_74 (25) = happyShift action_25
action_74 (26) = happyShift action_26
action_74 (27) = happyShift action_27
action_74 (28) = happyShift action_28
action_74 (42) = happyShift action_29
action_74 (44) = happyShift action_30
action_74 (7) = happyGoto action_19
action_74 (8) = happyGoto action_20
action_74 (15) = happyGoto action_21
action_74 (20) = happyGoto action_85
action_74 (21) = happyGoto action_23
action_74 (23) = happyGoto action_24
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_38

action_76 _ = happyReduce_37

action_77 _ = happyReduce_36

action_78 (43) = happyShift action_84
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (46) = happyShift action_83
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (24) = happyShift action_12
action_80 (25) = happyShift action_25
action_80 (26) = happyShift action_26
action_80 (27) = happyShift action_27
action_80 (28) = happyShift action_28
action_80 (42) = happyShift action_29
action_80 (44) = happyShift action_30
action_80 (7) = happyGoto action_19
action_80 (8) = happyGoto action_20
action_80 (15) = happyGoto action_21
action_80 (16) = happyGoto action_82
action_80 (20) = happyGoto action_39
action_80 (21) = happyGoto action_23
action_80 (23) = happyGoto action_24
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_24

action_82 _ = happyReduce_26

action_83 (24) = happyShift action_12
action_83 (6) = happyGoto action_96
action_83 (23) = happyGoto action_79
action_83 _ = happyReduce_5

action_84 (24) = happyShift action_12
action_84 (30) = happyShift action_13
action_84 (31) = happyShift action_14
action_84 (32) = happyShift action_15
action_84 (33) = happyShift action_16
action_84 (39) = happyShift action_17
action_84 (40) = happyShift action_74
action_84 (47) = happyShift action_95
action_84 (5) = happyGoto action_3
action_84 (10) = happyGoto action_94
action_84 (11) = happyGoto action_71
action_84 (12) = happyGoto action_72
action_84 (13) = happyGoto action_73
action_84 (14) = happyGoto action_5
action_84 (15) = happyGoto action_6
action_84 (17) = happyGoto action_7
action_84 (18) = happyGoto action_8
action_84 (19) = happyGoto action_9
action_84 (22) = happyGoto action_10
action_84 (23) = happyGoto action_11
action_84 _ = happyReduce_11

action_85 (35) = happyShift action_41
action_85 (36) = happyShift action_42
action_85 (38) = happyShift action_43
action_85 (50) = happyShift action_45
action_85 (51) = happyShift action_46
action_85 (52) = happyShift action_47
action_85 (53) = happyShift action_48
action_85 (54) = happyShift action_49
action_85 _ = happyReduce_22

action_86 _ = happyReduce_12

action_87 _ = happyReduce_27

action_88 (35) = happyShift action_41
action_88 (36) = happyShift action_42
action_88 (38) = happyShift action_43
action_88 (46) = happyShift action_93
action_88 (50) = happyShift action_45
action_88 (51) = happyShift action_46
action_88 (52) = happyShift action_47
action_88 (53) = happyShift action_48
action_88 (54) = happyShift action_49
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_8

action_90 (29) = happyShift action_91
action_90 (34) = happyShift action_92
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_29

action_92 (24) = happyShift action_12
action_92 (30) = happyShift action_13
action_92 (31) = happyShift action_14
action_92 (32) = happyShift action_15
action_92 (33) = happyShift action_16
action_92 (39) = happyShift action_17
action_92 (40) = happyShift action_74
action_92 (5) = happyGoto action_3
action_92 (10) = happyGoto action_100
action_92 (11) = happyGoto action_71
action_92 (12) = happyGoto action_72
action_92 (13) = happyGoto action_73
action_92 (14) = happyGoto action_5
action_92 (15) = happyGoto action_6
action_92 (17) = happyGoto action_7
action_92 (18) = happyGoto action_8
action_92 (19) = happyGoto action_9
action_92 (22) = happyGoto action_10
action_92 (23) = happyGoto action_11
action_92 _ = happyReduce_11

action_93 (24) = happyShift action_12
action_93 (25) = happyShift action_25
action_93 (26) = happyShift action_26
action_93 (27) = happyShift action_27
action_93 (28) = happyShift action_28
action_93 (42) = happyShift action_29
action_93 (44) = happyShift action_30
action_93 (7) = happyGoto action_19
action_93 (8) = happyGoto action_20
action_93 (15) = happyGoto action_21
action_93 (20) = happyGoto action_99
action_93 (21) = happyGoto action_23
action_93 (23) = happyGoto action_24
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (29) = happyShift action_98
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (24) = happyShift action_12
action_95 (30) = happyShift action_13
action_95 (31) = happyShift action_14
action_95 (32) = happyShift action_15
action_95 (33) = happyShift action_16
action_95 (39) = happyShift action_17
action_95 (40) = happyShift action_74
action_95 (5) = happyGoto action_3
action_95 (10) = happyGoto action_97
action_95 (11) = happyGoto action_71
action_95 (12) = happyGoto action_72
action_95 (13) = happyGoto action_73
action_95 (14) = happyGoto action_5
action_95 (15) = happyGoto action_6
action_95 (17) = happyGoto action_7
action_95 (18) = happyGoto action_8
action_95 (19) = happyGoto action_9
action_95 (22) = happyGoto action_10
action_95 (23) = happyGoto action_11
action_95 _ = happyReduce_11

action_96 _ = happyReduce_6

action_97 (29) = happyShift action_103
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_3

action_99 (35) = happyShift action_41
action_99 (36) = happyShift action_42
action_99 (38) = happyShift action_43
action_99 (43) = happyShift action_102
action_99 (50) = happyShift action_45
action_99 (51) = happyShift action_46
action_99 (52) = happyShift action_47
action_99 (53) = happyShift action_48
action_99 (54) = happyShift action_49
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (29) = happyShift action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_30

action_102 (47) = happyShift action_104
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_4

action_104 (41) = happyShift action_105
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (24) = happyShift action_12
action_105 (30) = happyShift action_13
action_105 (31) = happyShift action_14
action_105 (32) = happyShift action_15
action_105 (33) = happyShift action_16
action_105 (39) = happyShift action_17
action_105 (40) = happyShift action_74
action_105 (5) = happyGoto action_3
action_105 (10) = happyGoto action_106
action_105 (11) = happyGoto action_71
action_105 (12) = happyGoto action_72
action_105 (13) = happyGoto action_73
action_105 (14) = happyGoto action_5
action_105 (15) = happyGoto action_6
action_105 (17) = happyGoto action_7
action_105 (18) = happyGoto action_8
action_105 (19) = happyGoto action_9
action_105 (22) = happyGoto action_10
action_105 (23) = happyGoto action_11
action_105 _ = happyReduce_11

action_106 (29) = happyShift action_107
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_28

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 (
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 _
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_3 = happyReduce 7 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 8 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 (
	)

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 _
	_
	_
	 =  HappyAbsSyn6
		 (
	)

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	_
	_
	 =  HappyAbsSyn7
		 (
	)

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (
	)

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 _
	_
	 =  HappyAbsSyn9
		 (
	)

happyReduce_11 = happySpecReduce_0  10 happyReduction_11
happyReduction_11  =  HappyAbsSyn10
		 (
	)

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 _
	_
	 =  HappyAbsSyn10
		 (
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn11
		 (
	)

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn11
		 (
	)

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn12
		 (
	)

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn12
		 (
	)

happyReduce_17 = happySpecReduce_1  12 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn12
		 (
	)

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn12
		 (
	)

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn12
		 (
	)

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn12
		 (
	)

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn12
		 (
	)

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 _
	_
	 =  HappyAbsSyn13
		 (
	)

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 _
	_
	_
	 =  HappyAbsSyn14
		 (
	)

happyReduce_24 = happyReduce 4 15 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  16 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn16
		 (
	)

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 _
	_
	_
	 =  HappyAbsSyn16
		 (
	)

happyReduce_27 = happyReduce 5 17 happyReduction_27
happyReduction_27 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 12 18 happyReduction_28
happyReduction_28 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 6 19 happyReduction_29
happyReduction_29 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 8 19 happyReduction_30
happyReduction_30 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  20 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (\p -> happy_var_1 p + happy_var_3 p
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  20 happyReduction_32
happyReduction_32 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_33 = happySpecReduce_3  20 happyReduction_33
happyReduction_33 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_34 = happySpecReduce_3  20 happyReduction_34
happyReduction_34 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_35 = happySpecReduce_3  20 happyReduction_35
happyReduction_35 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_36 = happySpecReduce_3  20 happyReduction_36
happyReduction_36 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_37 = happySpecReduce_3  20 happyReduction_37
happyReduction_37 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_38 = happySpecReduce_3  20 happyReduction_38
happyReduction_38 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_39 = happySpecReduce_3  20 happyReduction_39
happyReduction_39 _
	_
	_
	 =  HappyAbsSyn20
		 (
	)

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn20
		 (
	)

happyReduce_41 = happySpecReduce_1  21 happyReduction_41
happyReduction_41 (HappyTerminal (TokenIntLit happy_var_1))
	 =  HappyAbsSyn21
		 (\p -> happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  21 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn21
		 (
	)

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn21
		 (
	)

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn21
		 (
	)

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn21
		 (
	)

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn21
		 (
	)

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn21
		 (
	)

happyReduce_48 = happyReduce 4 22 happyReduction_48
happyReduction_48 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_1  23 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn23
		 (
	)

happyNewToken action sts stk [] =
	action 55 55 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenId -> cont 24;
	TokenIntLit happy_dollar_dollar -> cont 25;
	TokenRealLit happy_dollar_dollar -> cont 26;
	TokenTrue -> cont 27;
	TokenFalse -> cont 28;
	TokenEnd -> cont 29;
	TokenDef -> cont 30;
	TokenWhile -> cont 31;
	TokenFor -> cont 32;
	TokenIf -> cont 33;
	TokenElse -> cont 34;
	TokenAnd -> cont 35;
	TokenOr -> cont 36;
	TokenNot -> cont 37;
	TokenXor -> cont 38;
	TokenPrint -> cont 39;
	TokenReturn -> cont 40;
	TokenNewline -> cont 41;
	TokenLParen -> cont 42;
	TokenRParen -> cont 43;
	TokenLBracket -> cont 44;
	TokenRBracket -> cont 45;
	TokenComma -> cont 46;
	TokenColon -> cont 47;
	TokenInRange -> cont 48;
	TokenEquals -> cont 49;
	TokenAdd -> cont 50;
	TokenSub -> cont 51;
	TokenMul -> cont 52;
	TokenDiv -> cont 53;
	TokenRem -> cont 54;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 55 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
toygrammar tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
data Exp  
    = Let String Exp Exp
    | Exp1 Exp1
    deriving Show

data Exp1 
    = Plus Exp1 Term 
    | Minus Exp1 Term 
    | Term Term
    deriving Show

data Term 
    = Times Term Factor 
    | Div Term Factor 
    | Factor Factor
    deriving Show

data Factor 
    = Int Int 
    | Var String 
    | Brack Exp
    deriving Show

data Token
    = TokenLet
    | TokenIntLit Int
    | TokenVar String
    | TokenRealLit Float
    | TokenTrue 
    | TokenFalse 
    | TokenEquals 
    | TokenAdd 
    | TokenSub 
    | TokenMul
    | TokenDiv 
    | TokenRem
    | TokenLParen 
    | TokenRParen 
    | TokenLBracket 
    | TokenRBracket 
    | TokenComma 
    | TokenColon 
    | TokenInRange
    | TokenNewline 
    | TokenEnd  
    | TokenDef 
    | TokenWhile 
    | TokenFor 
    | TokenIf 
    | TokenElse 
    | TokenAnd 
    | TokenOr 
    | TokenNot 
    | TokenXor 
    | TokenPrint  
    | TokenReturn
    | TokenId 
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    --   | isSpace c = lexer cs
    --   | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer (EQUALS:cs) = TokenEq : lexer cs
lexer (PLUS:cs)   = TokenPlus : lexer cs
lexer (MINUS:cs)  = TokenMinus : lexer cs
lexer (MUL:cs)    = TokenTimes : lexer cs
lexer (DIV:cs)    = TokenDiv : lexer cs
lexer (LPAREN:cs) = TokenLParen : lexer cs
lexer (RPAREN:cs) = TokenRParen : lexer cs

lexNum cs = TokenIntLit (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("in",rest)  -> TokenIn : lexer rest
      (var,rest)   -> TokenVar var : lexer rest

main = getContents >>= print . flip calc [] . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
