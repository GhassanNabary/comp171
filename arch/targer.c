#define DO_SHOW 1 
#define SOB_FALSE CONST_LIST+1
#include "cisc.h" 
#include "beneyal.h" 
int main() 
{ 
START_MACHINE; 
JUMP(CONTINUE); 
#include "char.lib" 
 #include "io.lib" 
 #include "math.lib" 
 #include "string.lib" 
 #include "system.lib" 
 #include "scheme.lib" 
CONTINUE: 
PUSH (IMM(5)); 
CALL (MALLOC); 
DROP (1); 
MOV (CONST_LIST,R0); 
MOV (R1,IMM(1));
PUSH (R1);
CALL (MAKE_SOB_BOOL);
DROP (1);
MOV (INDD(CONST_LIST,0),R0); 
MOV (R1,IMM(0));
PUSH (R1);
CALL (MAKE_SOB_BOOL);
DROP (1);
MOV (INDD(CONST_LIST,1),R0); 
CALL (MAKE_SOB_VOID);
MOV (INDD(CONST_LIST,2),R0); 
CALL (MAKE_SOB_NIL);
MOV (INDD(CONST_LIST,3),R0); 
MOV (R1,109);
PUSH (R1);
MOV (R1,99);
PUSH (R1);
MOV (R1,115);
PUSH (R1);
MOV (R1,46);
PUSH (R1);
MOV (R1,99);
PUSH (R1);
MOV (R1,114);
PUSH (R1);
MOV (R1,115);
PUSH (R1);
MOV (R1,7);
PUSH (R1);
CALL (MAKE_SOB_STRING);
DROP (8);
MOV (INDD(CONST_LIST,4),R0); 
MOV(R10,INDD(CONST_LIST,3));
PUSH(IMM(2));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0,0),INDD(CONST_LIST,4));
MOV(INDD(R0,1),R10);
MOV(R10,R0);
MOV(SYMBOL_LIST,R10);
PUSH (IMM(40)); 
CALL (MALLOC); 
DROP (1); 
MOV (FVAR_LIST,R0); 
JUMP (T_INTEGERPredicatJumpOverBodyLable411);
T_INTEGERPredicatBodyLable412:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_INTEGER);
JUMP_NE (T_INTEGERPredicatNotTrue410);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_INTEGERPredicatJumpEndBody409);
T_INTEGERPredicatNotTrue410:
MOV (R0,INDD(CONST_LIST,1));
T_INTEGERPredicatJumpEndBody409:
POP(FP);
RETURN;
T_INTEGERPredicatJumpOverBodyLable411:
PUSH (LABEL (T_INTEGERPredicatBodyLable412));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,0),R0);
JUMP (T_BOOLPredicatJumpOverBodyLable415);
T_BOOLPredicatBodyLable416:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_BOOL);
JUMP_NE (T_BOOLPredicatNotTrue414);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_BOOLPredicatJumpEndBody413);
T_BOOLPredicatNotTrue414:
MOV (R0,INDD(CONST_LIST,1));
T_BOOLPredicatJumpEndBody413:
POP(FP);
RETURN;
T_BOOLPredicatJumpOverBodyLable415:
PUSH (LABEL (T_BOOLPredicatBodyLable416));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,1),R0);
JUMP (T_CHARPredicatJumpOverBodyLable403);
T_CHARPredicatBodyLable404:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_CHAR);
JUMP_NE (T_CHARPredicatNotTrue402);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_CHARPredicatJumpEndBody401);
T_CHARPredicatNotTrue402:
MOV (R0,INDD(CONST_LIST,1));
T_CHARPredicatJumpEndBody401:
POP(FP);
RETURN;
T_CHARPredicatJumpOverBodyLable403:
PUSH (LABEL (T_CHARPredicatBodyLable404));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,2),R0);
JUMP (T_PAIRPredicatJumpOverBodyLable407);
T_PAIRPredicatBodyLable408:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_PAIR);
JUMP_NE (T_PAIRPredicatNotTrue406);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_PAIRPredicatJumpEndBody405);
T_PAIRPredicatNotTrue406:
MOV (R0,INDD(CONST_LIST,1));
T_PAIRPredicatJumpEndBody405:
POP(FP);
RETURN;
T_PAIRPredicatJumpOverBodyLable407:
PUSH (LABEL (T_PAIRPredicatBodyLable408));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,3),R0);
JUMP (T_NILPredicatJumpOverBodyLable395);
T_NILPredicatBodyLable396:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_NIL);
JUMP_NE (T_NILPredicatNotTrue394);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_NILPredicatJumpEndBody393);
T_NILPredicatNotTrue394:
MOV (R0,INDD(CONST_LIST,1));
T_NILPredicatJumpEndBody393:
POP(FP);
RETURN;
T_NILPredicatJumpOverBodyLable395:
PUSH (LABEL (T_NILPredicatBodyLable396));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,4),R0);
JUMP (T_STRINGPredicatJumpOverBodyLable399);
T_STRINGPredicatBodyLable400:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_STRING);
JUMP_NE (T_STRINGPredicatNotTrue398);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_STRINGPredicatJumpEndBody397);
T_STRINGPredicatNotTrue398:
MOV (R0,INDD(CONST_LIST,1));
T_STRINGPredicatJumpEndBody397:
POP(FP);
RETURN;
T_STRINGPredicatJumpOverBodyLable399:
PUSH (LABEL (T_STRINGPredicatBodyLable400));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,5),R0);
JUMP (T_SYMBOLPredicatJumpOverBodyLable387);
T_SYMBOLPredicatBodyLable388:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_SYMBOL);
JUMP_NE (T_SYMBOLPredicatNotTrue386);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_SYMBOLPredicatJumpEndBody385);
T_SYMBOLPredicatNotTrue386:
MOV (R0,INDD(CONST_LIST,1));
T_SYMBOLPredicatJumpEndBody385:
POP(FP);
RETURN;
T_SYMBOLPredicatJumpOverBodyLable387:
PUSH (LABEL (T_SYMBOLPredicatBodyLable388));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,6),R0);
JUMP (T_VECTORPredicatJumpOverBodyLable391);
T_VECTORPredicatBodyLable392:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_VECTOR);
JUMP_NE (T_VECTORPredicatNotTrue390);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_VECTORPredicatJumpEndBody389);
T_VECTORPredicatNotTrue390:
MOV (R0,INDD(CONST_LIST,1));
T_VECTORPredicatJumpEndBody389:
POP(FP);
RETURN;
T_VECTORPredicatJumpOverBodyLable391:
PUSH (LABEL (T_VECTORPredicatBodyLable392));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,7),R0);
JUMP (T_CLOSUREPredicatJumpOverBodyLable380);
T_CLOSUREPredicatBodyLable381:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,IND(R1));
CMP (R1,T_CLOSURE);
JUMP_NE (T_CLOSUREPredicatNotTrue379);
MOV (R0,INDD(CONST_LIST,0));
JUMP (T_CLOSUREPredicatJumpEndBody378);
T_CLOSUREPredicatNotTrue379:
MOV (R0,INDD(CONST_LIST,1));
T_CLOSUREPredicatJumpEndBody378:
POP(FP);
RETURN;
T_CLOSUREPredicatJumpOverBodyLable380:
PUSH (LABEL (T_CLOSUREPredicatBodyLable381));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,8),R0);
JUMP (consJumpOverBodyLable383);
consBodyLable384:
PUSH (FP);
MOV (FP,SP);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
PUSH (R2);
PUSH (R1);
CALL (MAKE_SOB_PAIR);
DROP (2);
POP(FP);
RETURN;
consJumpOverBodyLable383:
PUSH (LABEL (consBodyLable384));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,9),R0);
JUMP (zeroJumpOverBodyLable373);
zeroBodyLable374:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R1,INDD(R1,1));
CMP (R1,IMM(0));
JUMP_NE (zeroNotTrue372);
MOV (R0,INDD(CONST_LIST,0));
JUMP (zeroJumpEndBody371);
zeroNotTrue372:
MOV (R0,INDD(CONST_LIST,1));
zeroJumpEndBody371:
POP(FP);
RETURN;
zeroJumpOverBodyLable373:
PUSH (LABEL (zeroBodyLable374));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,10),R0);
JUMP (setcarJumpOverBodyLable376);
setcarBodyLable377:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
CMP (IND(R1),IMM(T_PAIR));
JUMP_NE (setcarERROR375);
MOV (INDD(R1,1),R2);
MOV (R0,INDD(CONST_LIST,2));
POP(FP);
RETURN;
setcarERROR375:
SHOW ("ERROR IN SET-CAR!",R1);
HALT;
setcarJumpOverBodyLable376:
PUSH (LABEL (setcarBodyLable377));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,11),R0);
JUMP (setcarJumpOverBodyLable366);
setcarBodyLable367:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
CMP (IND(R1),IMM(T_PAIR));
JUMP_NE (setcarERROR365);
MOV (INDD(R1,2),R2);
MOV (R0,INDD(CONST_LIST,2));
POP(FP);
RETURN;
setcarERROR365:
SHOW ("ERROR IN SET-CDR!",R1);
HALT;
setcarJumpOverBodyLable366:
PUSH (LABEL (setcarBodyLable367));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,12),R0);
JUMP (stringlengthJumpOverBodyLable369);
stringlengthBodyLable370:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),IMM(T_STRING));
JUMP_NE (stringlengthERROR368);
MOV (R1,INDD(R1,1));
PUSH (R1);
CALL (MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
stringlengthERROR368:
SHOW ("ERROR IN STRING-LENGTH",R1);
HALT;
stringlengthJumpOverBodyLable369:
PUSH (LABEL (stringlengthBodyLable370));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,13),R0);
JUMP (stringToSymbolJumpOverBodyLable360);
stringToSymbolBodyLable361:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),IMM(T_STRING));
JUMP_NE (stringToSymbolERROR359);
MOV (R2,IND(SYMBOL_LIST));
stringToSymbolDoLoop358:
CMP (IND(R2),IMM(T_NIL));
JUMP_EQ (stringToSymbolAddString357);
CMP (R2,IMM(T_NIL));
JUMP_EQ (stringToSymbolAddString357);
CMP (INDD(R2,1),INDD(R1,1));
JUMP_NE (stringToSymbolGoToTheNextString356);
MOV (R3,INDD(R2,1));
ADD (R3,IMM(1));
stringToSymbolCheckStringLoop355:
CMP (R3,IMM(-1));
JUMP_EQ (stringToSymbolGetSymbol354);
CMP (INDD(R2,R3),INDD(R1,R3));
JUMP_NE (stringToSymbolGoToTheNextString356);
SUB (R3,IMM(1));
JUMP (stringToSymbolCheckStringLoop355);
stringToSymbolGoToTheNextString356:
ADD (R2,IMM(1));
MOV (R2,IND(R2));
JUMP (stringToSymbolDoLoop358);
stringToSymbolGetSymbol354:
PUSH (2);
CALL (MALLOC);
DROP (1);
MOV (IND(R0),IMM(T_SYMBOL));
MOV (INDD(R0,1),R2);
JUMP (stringToSymbolEndFunctionLable353);
stringToSymbolAddString357:
PUSH (2);
CALL (MALLOC);
DROP (1);
MOV (IND(R0),R1);
ADD (R0,IMM(1));
MOV (IND(R0),SYMBOL_LIST);
SUB (R0,IMM(1));
MOV (SYMBOL_LIST,R0);
PUSH (2);
CALL (MALLOC);
DROP (1);
MOV (IND(R0),IMM(T_SYMBOL));
MOV (INDD(R0,1),IND(SYMBOL_LIST));
stringToSymbolEndFunctionLable353:
POP(FP);
RETURN;
stringToSymbolERROR359:
SHOW ("ERROR IN STRING->SYMBOL",R0);
HALT;
stringToSymbolJumpOverBodyLable360:
PUSH (LABEL (stringToSymbolBodyLable361));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,14),R0);
JUMP (symbolToStringJumpOverBodyLable363);
symbolToStringBodyLable364:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),T_SYMBOL);
JUMP_NE (symbolToStringERROR362);
MOV (R0,INDD(R1,1));
POP(FP);
RETURN;
symbolToStringERROR362:
SHOW ("ERROR IN SYMBOL->STRING",R0);
HALT;
symbolToStringJumpOverBodyLable363:
PUSH (LABEL (symbolToStringBodyLable364));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,15),R0);
JUMP (carAndCdrJumpOverBodyLable348);
carAndCdrBodyLable349:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),T_PAIR);
JUMP_NE (carAndCdrERROR347);
MOV (R0,INDD(R1,1));
POP(FP);
RETURN;
carAndCdrERROR347:
SHOW ("ERROR IN CAR",R0);
HALT;
carAndCdrJumpOverBodyLable348:
PUSH (LABEL (carAndCdrBodyLable349));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,16),R0);
JUMP (carAndCdrJumpOverBodyLable351);
carAndCdrBodyLable352:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),T_PAIR);
JUMP_NE (carAndCdrERROR350);
MOV (R0,INDD(R1,2));
POP(FP);
RETURN;
carAndCdrERROR350:
SHOW ("ERROR IN CDR",R0);
HALT;
carAndCdrJumpOverBodyLable351:
PUSH (LABEL (carAndCdrBodyLable352));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,17),R0);
JUMP (charToIntegerJumpOverBodyLable342);
charToIntegerBodyLable343:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),T_CHAR);
JUMP_NE (charToIntegerERROR341);
MOV (R1,INDD(R1,1));
PUSH (R1);
CALL (MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
charToIntegerERROR341:
SHOW ("ERROR IN CHAR->INTEGER",R0);
HALT;
charToIntegerJumpOverBodyLable342:
PUSH (LABEL (charToIntegerBodyLable343));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,18),R0);
JUMP (integerToCharJumpOverBodyLable345);
integerToCharBodyLable346:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),T_INTEGER);
JUMP_NE (integerToCharERROR344);
MOV (R1,INDD(R1,1));
PUSH (R1);
CALL (MAKE_SOB_CHAR);
DROP(1);
POP(FP);
RETURN;
integerToCharERROR344:
SHOW ("ERROR IN INTEGER->CHAR",R0);
HALT;
integerToCharJumpOverBodyLable345:
PUSH (LABEL (integerToCharBodyLable346));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,19),R0);
JUMP (makeStringJumpOverBodyLable332);
makeStringBodyLable333:
PUSH (FP);
MOV(FP, SP);
CMP (FPARG(1),IMM(1));
JUMP_NE (makeStringCheck2Args330);
MOV (R1,FPARG(2));
CMP (IND(R1),T_INTEGER);
JUMP_NE (makeStringERROR331);
MOV (R1,INDD(R1,1));
MOV (R3,R1);
MOV (R2,0);
JUMP (makeStringDoLoop329);
makeStringCheck2Args330:
CMP (FPARG(1),IMM(2));
JUMP_NE (makeStringERROR331);
MOV (R1,FPARG(2));
CMP (IND(R1),T_INTEGER);
JUMP_NE (makeStringERROR331);
MOV (R1,INDD(R1,1));
MOV (R3,R1);
MOV (R2,FPARG(3));
CMP (IND(R2),T_CHAR);
JUMP_NE (makeStringERROR331);
MOV (R2,INDD(R2,1));
makeStringDoLoop329:
CMP (R1,IMM(0));
JUMP_EQ (makeStringJumpOverLoop328);
PUSH (R2);
SUB (R1,IMM(1));
JUMP (makeStringDoLoop329);
makeStringJumpOverLoop328:
PUSH (R3);
CALL (MAKE_SOB_STRING);
DROP(1);
DROP (R3);
POP(FP);
RETURN;
makeStringERROR331:
SHOW ("ERROR IN MAKE-STRING",R0);
HALT;
makeStringJumpOverBodyLable332:
PUSH (LABEL (makeStringBodyLable333));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,20),R0);
JUMP (eqJumpOverLableBody339);
eqBodyLable340:
PUSH (FP);
MOV(FP, SP);
CMP (FPARG(1),IMM(2));
JUMP_NE (eqERROR338);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
CMP (IND(R1),IND(R2));
JUMP_NE (eqResultIsFalse337);
CMP (IND(R1),T_INTEGER);
JUMP_EQ (eqCheckContentIntSymbolChar336);
CMP (IND(R1),T_SYMBOL);
JUMP_EQ (eqCheckContentIntSymbolChar336);
CMP (IND(R1),T_CHAR);
JUMP_EQ (eqCheckContentIntSymbolChar336);
CMP (IND(R1),T_FRACTION);
JUMP_EQ (eqCheckContentFraction335);
CMP (R1,R2);
JUMP_NE (eqResultIsFalse337);
MOV (R0,INDD(CONST_LIST,0));
JUMP (eqEndFunctionLable334);
eqCheckContentIntSymbolChar336:
CMP (INDD(R1,1),INDD(R2,1));
JUMP_NE (eqResultIsFalse337);
MOV (R0,INDD(CONST_LIST,0));
JUMP (eqEndFunctionLable334);
eqCheckContentFraction335:
CMP (INDD(R1,1),INDD(R2,1));
JUMP_NE (eqResultIsFalse337);
CMP (INDD(R1,2),INDD(R2,2));
JUMP_NE (eqResultIsFalse337);
MOV (R0,INDD(CONST_LIST,0));
JUMP (eqEndFunctionLable334);
eqResultIsFalse337:
MOV (R0,INDD(CONST_LIST,1));
eqEndFunctionLable334:
POP(FP);
RETURN;
eqERROR338:
SHOW ("ERROR IN EQ",R0);
HALT;
eqJumpOverLableBody339:
PUSH (LABEL (eqBodyLable340));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,21),R0);
JUMP (stringRefJumpOverBodyLable313);
stringRefBodyLable314:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
CMP (IND(R1),T_STRING);
JUMP_NE (stringRefERROR312);
CMP (IND(R2),T_INTEGER);
JUMP_NE (stringRefERROR312);
MOV (R2,INDD(R2,1));
CMP (INDD(R1,1),R2);
JUMP_LE (stringRefERROR312);
MOV (R4,INDD(R1,1));
ADD (R4,IMM(1));
SUB (R4,R2);
MOV (R3,INDD(R1,R4));
PUSH (R3);
CALL (MAKE_SOB_CHAR);
DROP(1);
POP(FP);
RETURN;
stringRefERROR312:
SHOW ("ERROR IN STRING-REF",R0);
HALT;
stringRefJumpOverBodyLable313:
PUSH (LABEL (stringRefBodyLable314));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,22),R0);
JUMP (PlusBinaryJumpOverBodyLable320);
PlusBinaryBodyLable321:
PUSH (FP);
MOV(FP, SP);
MOV(R6,FPARG(2));
CMP (IND(R6),T_INTEGER);
JUMP_EQ (plusBinaryFirstArgumentIsNotFraction319);
CMP (IND(R6),T_FRACTION);
JUMP_NE (plusBinaryERROR315);
MOV (R1,INDD(R6,1));
MOV (R2,INDD(R6,2));
JUMP (plusBinaryJumpToSecondArgument317);
plusBinaryFirstArgumentIsNotFraction319:
MOV (R1,INDD(R6,1));
MOV (R2,1);
plusBinaryJumpToSecondArgument317:
MOV (R7,FPARG(3));
CMP (IND(R7),T_INTEGER);
JUMP_EQ (plusBinarySecondArgumentIsNotFraction318);
CMP (IND(R7),T_FRACTION);
JUMP_NE (plusBinaryERROR315);
MOV(R3,INDD(R7,1));
MOV(R4,INDD(R7,2));
JUMP (plusBinaryJumpEndFructionAssignment316);
plusBinarySecondArgumentIsNotFraction318:
MOV(R3,INDD(R7,1));
MOV(R4,1);
plusBinaryJumpEndFructionAssignment316:
MUL (R1,R4);
MUL (R3,R2);
ADD (R1,R3);
MUL (R2,R4);
MOV (R4,R1);
MOV (R5,R2);
CMP (R2,IMM(0));
JUMP_EQ (GCDInCISCERROR327);
GCD_LOOP325:
MOV (R3,R1);
REM (R3,R2);
CMP (R3,IMM(0));
JUMP_EQ (GCD_EXIT324);
MOV (R1,R2);
MOV (R2,R3);
JUMP (GCD_LOOP325);
GCD_EXIT324:
MOV (R0,R2);
JUMP (GCDJumpToEndFunction326);
GCDInCISCERROR327:
SHOW ("ERROR IN GCDInCISC",R0);
HALT;
GCDJumpToEndFunction326:
DIV (R4,R0);
DIV (R5,R0);
CMP (R5,IMM(1));
JUMP_EQ (runtimeMakeSobFractionMakeInteger323);
PUSH (IMM(3));
CALL (MALLOC);
DROP (1);
MOV (IND(R0),IMM(T_FRACTION));
MOV (INDD(R0,1),R4);
MOV (INDD(R0,2),R5);
JUMP (runtimeMakeSobFractionJumpEnd322);
runtimeMakeSobFractionMakeInteger323:
PUSH (R4);
CALL (MAKE_SOB_INTEGER);
DROP (1);
runtimeMakeSobFractionJumpEnd322:
POP(FP);
RETURN;
plusBinaryERROR315:
SHOW ("ERROR IN PLUSBINARY",R0);
HALT;
PlusBinaryJumpOverBodyLable320:
PUSH (LABEL (PlusBinaryBodyLable321));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,23),R0);
JUMP (minusBinaryJumpOverBodyLable291);
minusBinaryBodyLable292:
PUSH (FP);
MOV(FP, SP);
MOV(R6,FPARG(2));
CMP (IND(R6),T_INTEGER);
JUMP_EQ (minusBinaryFirstArgumentIsNotFraction290);
CMP (IND(R6),T_FRACTION);
JUMP_NE (minusBinaryERROR286);
MOV (R1,INDD(R6,1));
MOV (R2,INDD(R6,2));
JUMP (minusBinaryJumpToSecondArgument288);
minusBinaryFirstArgumentIsNotFraction290:
MOV (R1,INDD(R6,1));
MOV (R2,1);
minusBinaryJumpToSecondArgument288:
MOV (R7,FPARG(3));
CMP (IND(R7),T_INTEGER);
JUMP_EQ (minusBinarySecondArgumentIsNotFraction289);
CMP (IND(R7),T_FRACTION);
JUMP_NE (minusBinaryERROR286);
MOV(R3,INDD(R7,1));
MOV(R4,INDD(R7,2));
JUMP (minusBinaryJumpEndFructionAssignment287);
minusBinarySecondArgumentIsNotFraction289:
MOV(R3,INDD(R7,1));
MOV(R4,1);
minusBinaryJumpEndFructionAssignment287:
MUL (R1,R4);
MUL (R3,R2);
SUB (R1,R3);
MUL (R2,R4);
MOV (R4,R1);
MOV (R5,R2);
CMP (R2,IMM(0));
JUMP_EQ (GCDInCISCERROR298);
GCD_LOOP296:
MOV (R3,R1);
REM (R3,R2);
CMP (R3,IMM(0));
JUMP_EQ (GCD_EXIT295);
MOV (R1,R2);
MOV (R2,R3);
JUMP (GCD_LOOP296);
GCD_EXIT295:
MOV (R0,R2);
JUMP (GCDJumpToEndFunction297);
GCDInCISCERROR298:
SHOW ("ERROR IN GCDInCISC",R0);
HALT;
GCDJumpToEndFunction297:
DIV (R4,R0);
DIV (R5,R0);
CMP (R5,IMM(1));
JUMP_EQ (runtimeMakeSobFractionMakeInteger294);
PUSH (IMM(3));
CALL (MALLOC);
DROP (1);
MOV (IND(R0),IMM(T_FRACTION));
MOV (INDD(R0,1),R4);
MOV (INDD(R0,2),R5);
JUMP (runtimeMakeSobFractionJumpEnd293);
runtimeMakeSobFractionMakeInteger294:
PUSH (R4);
CALL (MAKE_SOB_INTEGER);
DROP (1);
runtimeMakeSobFractionJumpEnd293:
POP(FP);
RETURN;
minusBinaryERROR286:
INFO
SHOW ("ERROR IN MINUSBINARY",R0);
HALT;
minusBinaryJumpOverBodyLable291:
PUSH (LABEL (minusBinaryBodyLable292));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,24),R0);
JUMP (multiBinaryJumpOverBodyLable304);
multiBinaryBodyLable305:
PUSH (FP);
MOV(FP, SP);
MOV(R6,FPARG(2));
CMP (IND(R6),T_INTEGER);
JUMP_EQ (multiBinaryFirstArgumentIsNotFraction303);
CMP (IND(R6),T_FRACTION);
JUMP_NE (multiBinaryERROR299);
MOV (R1,INDD(R6,1));
MOV (R2,INDD(R6,2));
JUMP (multiBinaryJumpToSecondArgument301);
multiBinaryFirstArgumentIsNotFraction303:
MOV (R1,INDD(R6,1));
MOV (R2,1);
multiBinaryJumpToSecondArgument301:
MOV (R7,FPARG(3));
CMP (IND(R7),T_INTEGER);
JUMP_EQ (multiBinarySecondArgumentIsNotFraction302);
CMP (IND(R7),T_FRACTION);
JUMP_NE (multiBinaryERROR299);
MOV(R3,INDD(R7,1));
MOV(R4,INDD(R7,2));
JUMP (multiBinaryJumpEndFructionAssignment300);
multiBinarySecondArgumentIsNotFraction302:
MOV(R3,INDD(R7,1));
MOV(R4,1);
multiBinaryJumpEndFructionAssignment300:
MUL (R1,R3);
MUL (R2,R4);
MOV (R4,R1);
MOV (R5,R2);
CMP (R2,IMM(0));
JUMP_EQ (GCDInCISCERROR311);
GCD_LOOP309:
MOV (R3,R1);
REM (R3,R2);
CMP (R3,IMM(0));
JUMP_EQ (GCD_EXIT308);
MOV (R1,R2);
MOV (R2,R3);
JUMP (GCD_LOOP309);
GCD_EXIT308:
MOV (R0,R2);
JUMP (GCDJumpToEndFunction310);
GCDInCISCERROR311:
SHOW ("ERROR IN GCDInCISC",R0);
HALT;
GCDJumpToEndFunction310:
DIV (R4,R0);
DIV (R5,R0);
CMP (R5,IMM(1));
JUMP_EQ (runtimeMakeSobFractionMakeInteger307);
PUSH (IMM(3));
CALL (MALLOC);
DROP (1);
MOV (IND(R0),IMM(T_FRACTION));
MOV (INDD(R0,1),R4);
MOV (INDD(R0,2),R5);
JUMP (runtimeMakeSobFractionJumpEnd306);
runtimeMakeSobFractionMakeInteger307:
PUSH (R4);
CALL (MAKE_SOB_INTEGER);
DROP (1);
runtimeMakeSobFractionJumpEnd306:
POP(FP);
RETURN;
multiBinaryERROR299:
INFO
SHOW ("ERROR IN MULTIBINARY",R0);
HALT;
multiBinaryJumpOverBodyLable304:
PUSH (LABEL (multiBinaryBodyLable305));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,25),R0);
JUMP (divBinaryJumpOverBodyLable269);
divBinaryBodyLable270:
PUSH (FP);
MOV(FP, SP);
MOV(R6,FPARG(2));
CMP (IND(R6),T_INTEGER);
JUMP_EQ (divBinaryFirstArgumentIsNotFraction268);
CMP (IND(R6),T_FRACTION);
JUMP_NE (divBinaryERROR264);
MOV (R1,INDD(R6,1));
MOV (R2,INDD(R6,2));
JUMP (divBinaryJumpToSecondArgument266);
divBinaryFirstArgumentIsNotFraction268:
MOV (R1,INDD(R6,1));
MOV (R2,1);
divBinaryJumpToSecondArgument266:
MOV (R7,FPARG(3));
CMP (IND(R7),T_INTEGER);
JUMP_EQ (divBinarySecondArgumentIsNotFraction267);
CMP (IND(R7),T_FRACTION);
JUMP_NE (divBinaryERROR264);
MOV(R3,INDD(R7,1));
MOV(R4,INDD(R7,2));
JUMP (divBinaryJumpEndFructionAssignment265);
divBinarySecondArgumentIsNotFraction267:
MOV(R3,INDD(R7,1));
MOV(R4,1);
divBinaryJumpEndFructionAssignment265:
MOV (R5,R3);
MOV (R3,R4);
MOV (R4,R5);
MUL (R1,R3);
MUL (R2,R4);
MOV (R4,R1);
MOV (R5,R2);
CMP (R2,IMM(0));
JUMP_EQ (GCDInCISCERROR276);
GCD_LOOP274:
MOV (R3,R1);
REM (R3,R2);
CMP (R3,IMM(0));
JUMP_EQ (GCD_EXIT273);
MOV (R1,R2);
MOV (R2,R3);
JUMP (GCD_LOOP274);
GCD_EXIT273:
MOV (R0,R2);
JUMP (GCDJumpToEndFunction275);
GCDInCISCERROR276:
SHOW ("ERROR IN GCDInCISC",R0);
HALT;
GCDJumpToEndFunction275:
DIV (R4,R0);
DIV (R5,R0);
CMP (R5,IMM(1));
JUMP_EQ (runtimeMakeSobFractionMakeInteger272);
PUSH (IMM(3));
CALL (MALLOC);
DROP (1);
MOV (IND(R0),IMM(T_FRACTION));
MOV (INDD(R0,1),R4);
MOV (INDD(R0,2),R5);
JUMP (runtimeMakeSobFractionJumpEnd271);
runtimeMakeSobFractionMakeInteger272:
PUSH (R4);
CALL (MAKE_SOB_INTEGER);
DROP (1);
runtimeMakeSobFractionJumpEnd271:
POP(FP);
RETURN;
divBinaryERROR264:
INFO
SHOW ("ERROR IN DIVBINARY",R0);
HALT;
divBinaryJumpOverBodyLable269:
PUSH (LABEL (divBinaryBodyLable270));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,26),R0);
JUMP (bigerThenBinaryJumpOverBodyLable284);
bigerThenBinaryBodyLable285:
PUSH (FP);
MOV(FP, SP);
MOV(R6,FPARG(2));
CMP (IND(R6),T_INTEGER);
JUMP_EQ (bigerThenBinaryFirstArgumentIsNotFraction283);
CMP (IND(R6),T_FRACTION);
JUMP_NE (bigerThenBinaryERROR277);
MOV (R1,INDD(R6,1));
MOV (R2,INDD(R6,2));
JUMP (bigerThenBinaryJumpToSecondArgument281);
bigerThenBinaryFirstArgumentIsNotFraction283:
MOV (R1,INDD(R6,1));
MOV (R2,1);
bigerThenBinaryJumpToSecondArgument281:
MOV (R7,FPARG(3));
CMP (IND(R7),T_INTEGER);
JUMP_EQ (bigerThenBinarySecondArgumentIsNotFraction282);
CMP (IND(R7),T_FRACTION);
JUMP_NE (bigerThenBinaryERROR277);
MOV(R3,INDD(R7,1));
MOV(R4,INDD(R7,2));
JUMP (bigerThenBinaryJumpEndFructionAssignment280);
bigerThenBinarySecondArgumentIsNotFraction282:
MOV(R3,INDD(R7,1));
MOV(R4,1);
bigerThenBinaryJumpEndFructionAssignment280:
MUL (R1,R4);
MUL (R3,R2);
MUL (R2,R4);
MOV (R4,R2);
CMP (R1,R3);
JUMP_GT (bigerThenBinaryReturnTrue279);
MOV (R0,INDD(CONST_LIST,1));
JUMP (bigerThenBinaryEndFunction278);
bigerThenBinaryReturnTrue279:
MOV (R0,INDD(CONST_LIST,0));
bigerThenBinaryEndFunction278:
POP(FP);
RETURN;
bigerThenBinaryERROR277:
INFO
SHOW ("ERROR IN BIGER-THEN-BINARY",R0);
HALT;
bigerThenBinaryJumpOverBodyLable284:
PUSH (LABEL (bigerThenBinaryBodyLable285));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,27),R0);
JUMP (smallerThenBinaryJumpOverBodyLable253);
smallerThenBinaryBodyLable254:
PUSH (FP);
MOV(FP, SP);
MOV(R6,FPARG(2));
CMP (IND(R6),T_INTEGER);
JUMP_EQ (smallerThenBinaryFirstArgumentIsNotFraction252);
CMP (IND(R6),T_FRACTION);
JUMP_NE (smallerThenBinaryERROR246);
MOV (R1,INDD(R6,1));
MOV (R2,INDD(R6,2));
JUMP (smallerThenBinaryJumpToSecondArgument250);
smallerThenBinaryFirstArgumentIsNotFraction252:
MOV (R1,INDD(R6,1));
MOV (R2,1);
smallerThenBinaryJumpToSecondArgument250:
MOV (R7,FPARG(3));
CMP (IND(R7),T_INTEGER);
JUMP_EQ (smallerThenBinarySecondArgumentIsNotFraction251);
CMP (IND(R7),T_FRACTION);
JUMP_NE (smallerThenBinaryERROR246);
MOV(R3,INDD(R7,1));
MOV(R4,INDD(R7,2));
JUMP (smallerThenBinaryJumpEndFructionAssignment249);
smallerThenBinarySecondArgumentIsNotFraction251:
MOV(R3,INDD(R7,1));
MOV(R4,1);
smallerThenBinaryJumpEndFructionAssignment249:
MUL (R1,R4);
MUL (R3,R2);
MUL (R2,R4);
MOV (R4,R2);
CMP (R1,R3);
JUMP_LT (smallerThenBinaryReturnTrue248);
MOV (R0,INDD(CONST_LIST,1));
JUMP (smallerThenBinaryEndFunction247);
smallerThenBinaryReturnTrue248:
MOV (R0,INDD(CONST_LIST,0));
smallerThenBinaryEndFunction247:
POP(FP);
RETURN;
smallerThenBinaryERROR246:
INFO
SHOW ("ERROR IN SMALLER-THEN-BINARY",R0);
HALT;
smallerThenBinaryJumpOverBodyLable253:
PUSH (LABEL (smallerThenBinaryBodyLable254));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,28),R0);
JUMP (equalToBinaryJumpOverBodyLable262);
equalToBinaryBodyLable263:
PUSH (FP);
MOV(FP, SP);
MOV(R6,FPARG(2));
CMP (IND(R6),T_INTEGER);
JUMP_EQ (equalToBinaryFirstArgumentIsNotFraction261);
CMP (IND(R6),T_FRACTION);
JUMP_NE (equalToBinaryERROR255);
MOV (R1,INDD(R6,1));
MOV (R2,INDD(R6,2));
JUMP (equalToBinaryJumpToSecondArgument259);
equalToBinaryFirstArgumentIsNotFraction261:
MOV (R1,INDD(R6,1));
MOV (R2,1);
equalToBinaryJumpToSecondArgument259:
MOV (R7,FPARG(3));
CMP (IND(R7),T_INTEGER);
JUMP_EQ (equalToBinarySecondArgumentIsNotFraction260);
CMP (IND(R7),T_FRACTION);
JUMP_NE (equalToBinaryERROR255);
MOV(R3,INDD(R7,1));
MOV(R4,INDD(R7,2));
JUMP (equalToBinaryJumpEndFructionAssignment258);
equalToBinarySecondArgumentIsNotFraction260:
MOV(R3,INDD(R7,1));
MOV(R4,1);
equalToBinaryJumpEndFructionAssignment258:
MUL (R1,R4);
MUL (R3,R2);
MUL (R2,R4);
MOV (R4,R2);
CMP (R1,R3);
JUMP_EQ (equalToBinaryReturnTrue257);
MOV (R0,INDD(CONST_LIST,1));
JUMP (equalToBinaryEndFunction256);
equalToBinaryReturnTrue257:
MOV (R0,INDD(CONST_LIST,0));
equalToBinaryEndFunction256:
POP(FP);
RETURN;
equalToBinaryERROR255:
INFO
SHOW ("ERROR IN EQUAL-TO-BINARY",R0);
HALT;
equalToBinaryJumpOverBodyLable262:
PUSH (LABEL (equalToBinaryBodyLable263));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,29),R0);
JUMP (vectorLengthJumpOverBodyImplemenatation241);
vectorLengthBodyLabel240:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP(IND(R1),T_VECTOR);
JUMP_NE(vectorLengthERROR239);
MOV(R2,INDD(R1,1));
PUSH(R2);
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
vectorLengthERROR239:
SHOW ("ERROR IN VECTOR_LENGTH",R0);
HALT;
vectorLengthJumpOverBodyImplemenatation241:
PUSH (LABEL (vectorLengthBodyLabel240));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,30),R0);
JUMP (vectorImplementationJumpOverBodeyLabel245);
vectorImplementationBodyLabel244:
PUSH (FP);
MOV(FP, SP);
MOV(R1,FPARG(1));
MOV(R2,0);
MOV(R10,2);
vectorImplementationStartLoopLabel243:
CMP(R2,R1);
JUMP_EQ(vectorImplementationEndLoopLabel242);
MOV(R3,FPARG(R10));
PUSH(R3);
ADD(R10,1);
ADD(R2,1);
JUMP(vectorImplementationStartLoopLabel243);
vectorImplementationEndLoopLabel242:
PUSH(R1);
CALL(MAKE_SOB_VECTOR);
POP(R1);
DROP(R1);
POP(FP);
RETURN;
vectorImplementationJumpOverBodeyLabel245:
PUSH (LABEL (vectorImplementationBodyLabel244));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,31),R0);
JUMP (vectorRefJumpOverBodyLable234);
vectorRefBodyLable235:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
CMP (IND(R1),T_VECTOR);
JUMP_NE (vectorRefERROR233);
CMP (IND(R2),T_INTEGER);
JUMP_NE (vectorRefERROR233);
MOV (R2,INDD(R2,1));
CMP (INDD(R1,1),R2);
JUMP_LE (vectorRefERROR233);
ADD(R2,2);
MOV (R0,INDD(R1,R2));
POP(FP);
RETURN;
vectorRefERROR233:
SHOW ("ERROR IN VECTOR-REF",R0);
HALT;
vectorRefJumpOverBodyLable234:
PUSH (LABEL (vectorRefBodyLable235));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,32),R0);
JUMP (vectorStJumpOverBodyLable237);
vectorSetBodyLable238:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
MOV (R3,FPARG(4));
CMP (IND(R1),T_VECTOR);
JUMP_NE (vectorSetERROR236);
CMP (IND(R2),T_INTEGER);
JUMP_NE (vectorSetERROR236);
MOV (R2,INDD(R2,1));
CMP (INDD(R1,1),R2);
JUMP_LE (vectorSetERROR236);
ADD(R2,2);
MOV (INDD(R1,R2),R3);
MOV (R0,INDD(CONST_LIST,2));
POP(FP);
RETURN;
vectorSetERROR236:
SHOW ("ERROR IN VECTOR-SET!",R0);
HALT;
vectorStJumpOverBodyLable237:
PUSH (LABEL (vectorSetBodyLable238));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,33),R0);
JUMP (deNumeratorImplementationJumpOverBodyLabel229);
deNumeratorImplementationBodyLabel228:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP(INDD(R1,1),T_INTEGER);
JUMP_EQ(deNumeratorImplementationIntegerLabel227);
MOV(R3,INDD(R1,2));
JUMP(deNumearatorCallMakeSobLabel226);
deNumeratorImplementationIntegerLabel227:
MOV(R3,IMM(1));
deNumearatorCallMakeSobLabel226:
PUSH(R3);
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
deNumeratorImplementationJumpOverBodyLabel229:
PUSH (LABEL (deNumeratorImplementationBodyLabel228));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,34),R0);
JUMP (numeratorImplementationJumpOverBodyLabel231);
numeratorImplementationBodyLabel230:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV(R3,INDD(R1,1));
PUSH(R3);
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
numeratorImplementationJumpOverBodyLabel231:
PUSH (LABEL (numeratorImplementationBodyLabel230));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,35),R0);
JUMP (remainderJumpOverBodyLable220);
remainderBodyLable221:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
MOV (R2,FPARG(3));
CMP (IND(R1),T_INTEGER);
JUMP_NE (remainderERROR219);
CMP (IND(R2),T_INTEGER);
JUMP_NE (remainderERROR219);
MOV (R1,INDD(R1,1));
MOV (R2,INDD(R2,1));
REM (R1,R2);
PUSH (R1);
CALL (MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
remainderERROR219:
SHOW ("ERROR IN REMAINDER",R0);
HALT;
remainderJumpOverBodyLable220:
PUSH (LABEL (remainderBodyLable221));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,36),R0);
JUMP (numberOrRationalJumpOverBodyLable224);
numberOrRationalBodyLable225:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),T_INTEGER);
JUMP_EQ (numberOrRationalReturnTrue223);
CMP (IND(R1),T_FRACTION);
JUMP_EQ (numberOrRationalReturnTrue223);
MOV (R0,INDD(CONST_LIST,1));
JUMP (numberOrRationalJumpEndFunction222);
numberOrRationalReturnTrue223:
MOV (R0,INDD(CONST_LIST,0));
numberOrRationalJumpEndFunction222:
POP(FP);
RETURN;
numberOrRationalJumpOverBodyLable224:
PUSH (LABEL (numberOrRationalBodyLable225));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,37),R0);
JUMP (numberOrRationalJumpOverBodyLable211);
numberOrRationalBodyLable212:
PUSH (FP);
MOV(FP, SP);
MOV (R1,FPARG(2));
CMP (IND(R1),T_INTEGER);
JUMP_EQ (numberOrRationalReturnTrue210);
CMP (IND(R1),T_FRACTION);
JUMP_EQ (numberOrRationalReturnTrue210);
MOV (R0,INDD(CONST_LIST,1));
JUMP (numberOrRationalJumpEndFunction209);
numberOrRationalReturnTrue210:
MOV (R0,INDD(CONST_LIST,0));
numberOrRationalJumpEndFunction209:
POP(FP);
RETURN;
numberOrRationalJumpOverBodyLable211:
PUSH (LABEL (numberOrRationalBodyLable212));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,38),R0);
JUMP (makeVectorJumpOverBodyLable217);
makeVectorBodyLable218:
PUSH (FP);
MOV(FP, SP);
CMP (FPARG(1),IMM(1));
JUMP_NE (makeVectorCheck2Args215);
MOV (R1,FPARG(2));
CMP (IND(R1),T_INTEGER);
JUMP_NE (makeVectorERROR216);
MOV (R1,INDD(R1,1));
MOV (R3,R1);
MOV (R2,0);
PUSH (R2);
CALL (MAKE_SOB_INTEGER);
DROP (1);
MOV (R2,R0);
JUMP (makeVectorDoLoop214);
makeVectorCheck2Args215:
CMP (FPARG(1),IMM(2));
JUMP_NE (makeVectorERROR216);
MOV (R1,FPARG(2));
CMP (IND(R1),T_INTEGER);
JUMP_NE (makeVectorERROR216);
MOV (R1,INDD(R1,1));
MOV (R3,R1);
MOV (R2,FPARG(3));
makeVectorDoLoop214:
CMP (R1,IMM(0));
JUMP_EQ (makeVectorJumpOverLoop213);
PUSH (R2);
SUB (R1,IMM(1));
JUMP (makeVectorDoLoop214);
makeVectorJumpOverLoop213:
PUSH (R3);
CALL (MAKE_SOB_VECTOR);
POP(R3);
DROP (R3);
POP(FP);
RETURN;
makeVectorERROR216:
SHOW ("ERROR IN MAKE-VECTOR",R0);
HALT;
makeVectorJumpOverBodyLable217:
PUSH (LABEL (makeVectorBodyLable218));
PUSH (IMM(0));
CALL (MAKE_SOB_CLOSURE);
DROP (2);
MOV (INDD(FVAR_LIST,39),R0);
MOV (R0,CONST_LIST);
ADD (R0,IMM(4));
MOV (R0,IND(R0)); 
INFO 
STOP_MACHINE; 
return 0; 
} 