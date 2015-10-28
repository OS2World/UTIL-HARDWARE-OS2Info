{&G5+}
{$B-,D+,H-,I+,J+,P-,Q+,R+,S+,T+,V+,W-,X+,Z-}
{&AlignCode+,AlignData+,AlignRec-,Asm+,Cdecl-,Comments-,Delphi+,Frame+}
{&LocInfo+,Open32-,Optimise+,OrgName-,SmartLink-,Speed+,Use32-,ZD+}
{$M 32768}

(*
   Description.: Program to read available operating system and hardware details.
                 The long description can be found below.

   Author......: Juergen Ulbts, Germany
   Copyright...: 2004-2006 - Juergen Ulbts, Germany
   Contact.....: eMail     - info@juergen-ulbts.de    (or what can be found on my website)
                 Website   - http://www.juergen-ulbts.de

   Date........: 2006-06-01
   Version.....: 0.41

   LAST CHANGES:
       J.U. - 2006-06-01 - FIRST OFFICIAL RELEASE
                         - added the updated Dos16MemAvail call wrapper. Thank you again Veit!
                           This one doesn't need a dll!
       J.U. - 2006-05-30 - added call to Dos16MemAvail to be able to show the
                           available memory which can't be accessed by the other 32Bit DOS... calls.
                           This one uses a wrapper dll!
                           Many thanks go to Veit Kannegieser for his time!
                         - a little cleanup
       J.U. - 2004-07-28 - tried to fix CPU model, stepping,...
                           -> work in progress not yet working and
                              I don't know why :(
       J.U. - 2004-07-28 - updated Intel Processor names list (see IntelNames)
       J.U. - 2004-07-28 - got Keld's CompVal working with a workaround
       J.U. - 2004-07-26 - added Keld's CompVal and updated getNetworkInfo() to use it
       J.U. - 2004-07-20 - added getNetworkInfo() - need to fix traffic overflow (bytes)
       J.U. - 2004-07-20 - added getPhyDriveList by running RMVIEW
       J.U. - 2004-07-19 - updated reporting of OS main version (OS/2 or eCS)
       J.U. - 2004-07-16 - added SWAPPER.DAT uptime
       J.U. - 2004-07-11 - added CPU speed calculation
       J.U. - 2004-07-10 - added CPU detection
       J.U. - 2004-07-09 - added Partition reporting
       J.U. - 2004-07-08 - added OS/Kernel/number of processors/amount of RAM/...
       J.U. - 2004-07-05 - added uptime calculation from Dink and the default OS/2 way
                           (incl. the overflow after 49.7 days with the OS2 uptime counter)

   TODO: - add...: PCI-Devicelist
         - add...: read ServiceLevel from syslevel.os2 or syslevel.ecs (if available)
         - add...: report more information about Intel processors (Cache,...) and check Athlon cache report
         - add...: VIA CPU information
         - fix...: CPU model,stepping,... routine - currently most are always 0 which can't be correct
         - fix...: read correct location of SWAPPER.DAT from CONFIG.SYS "SWAPPATH" variable
         - change: remove REXX execution on SWAPPER.DAT to get size and replace it with API calls (currently does't work)
         - change: remove REXX execution on RMVIEW output and read the drive information directly by calling "RESMGR$"
         - change: remove REXX execution on Network Interfaces and Traffic - use API calls instead
         - change: reporting of uptime...use uptime1 and 3 to check if uptime2 is still valid...then use uptime1 and 3
                   to report the correct uptime value
         - change: split the code to several units !!!
         - clean.: check uses... for not needed units !!!
*)

PROGRAM OS2INFO;
uses long32, os2base, os2def, SysUtils, VPUtils, dos, vpSysLow, INT64, CRT, OS2Exec, Rexx;

TYPE
  TQWord = Comp;

VAR
      intCounter : Integer;
       ParamStrI : String;
        ProcName : string[ 12 ];
     ExtProcName : string[ 48 ];
             typ,                // for Intel only
      generation,                // for AMD only
   family, model,
        stepping : byte;
        ProcType : string[ 22 ];
    ExtInfo, Txt : String;
         MMXcode,
        DNowcode : boolean;       // presence of extended multimedia support
       MaxIDcode,                 // check if $8000000x instructions are available
     L1DataCache,
     L1codeCache,
         L2Cache : Word; // dword -- http://www.geocities.com/SiliconValley/Park/3230/pas/pasl2014.html
    rexxRxString : String;



CONST
  MaxRexxSrcBuffer      = 4096;
  crlf = chr(13)+chr(10);
  //-------------- Intel processor info ----------------
  IntelNames : array[0..31] of string[60] =
                                    ('Intel486 DX                                                 ',   //  0
                                     'Intel486 SX                                                 ',   //  1
                                     'Intel487                                                    ',   //  2
                                     'IntelDX2                                                    ',   //  3
                                     'IntelDX2 OverDrive                                          ',   //  4
                                     'Intel486 SL                                                 ',   //  5
                                     'IntelSX2                                                    ',   //  6
                                     'Write-Back Enh. IntelDX2                                    ',   //  7
                                     'IntelDX4                                                    ',   //  8
                                     'IntelDX4 OverDrive                                          ',   //  9
                                     'Pentium (60, 66 MHz)                                        ',   // 10
                                     'Pentium (75 - 200 MHz)                                      ',   // 11
                                     'Pentium OverDrive (60, 66 MHz)                              ',   // 12
                                     'Pentium OverDrive                                           ',   // 13
                                     'Pentium OverDrive for I486                                  ',   // 14
                                     'Pentium with MMX (166, 200 MHz)                             ',   // 15
                                     'Pentium OverDrive MMX (75 - 133 MHz)                        ',   // 16
                                     'Pentium Pro processor                                       ',   // 17
                                     'Pentium II processor, model 3                               ',   // 18
                                     'Pentium II/PII-Xeon/Celeron, model 5                        ',   // 19
                                     'Celeron processor, model 6                                  ',   // 20
                                     'Pentium III/PIII-Xeon, model 7                              ',   // 21
                                     'Pentium III/PIII-Xeon/Celeron, model 8                      ',   // 22
                                     'Intel Pentium M processor, model 9                          ',   // 23
                                     'Pentium III Xeon, model A                                   ',   // 24
                                     'Pentium III processor, model B                              ',   // 25
                                     'Intel Pentium M processor, model D, 90nm                    ',   // 26
                                     'Pentium II OverDrive processor                              ',   // 27
                                     'Pentium 4/Xeon, model 0 in 0.18 um                          ',   // 28
                                     'Pentium 4/Xeon, Celeron, model 1, 0.18 um                   ',   // 29
                                     'Pentium 4/Mobile P4-M/Xeon/Celeron(mobile), model 2, 0.13 um',   // 30
                                     'Pentium 4/Mobile P4-M/Xeon/Celeron, model 3, 90nm           ');  // 31
  IntelTypes : array[ 0..3 ] of string[ 22 ] =
                                     ('Original OEM processor',
                                      'OverDrive processor   ',
                                      'Dual processor        ',
                                      'Intel reserved        ');

//==============Helper Routine for DOS16 call================================
{&OrgName+}
function WrapDos16MemAvail: Longint; external;
{$L a.obj}
{&OrgName-}
//===========================================================================


//==============HelperRoutines===============================================
FUNCTION leadingzero(w: longint): String;
VAR
  s: String;
BEGIN
  s:=ulong2str(w);
  IF length(s) = 1 THEN s:='0'+s;
  leadingzero:=s;
END;



FUNCTION Double2Quad(L: longint): comp; assembler;
  asm // thx Keld!
  PUSH    0
  PUSH    L
  FILD    QWORD PTR [ESP]
  POP     EAX
  POP     EAX
END;

FUNCTION StrReplace(str:String; search:String; replacement:String):String;
VAR
  iCounter                        : Integer;
  strLength,strPosition           : Integer;
  searchLength, replacementLength : Integer;
BEGIN
  strLength         := LENGTH(str);
  searchLength      := LENGTH(search);
  replacementLength := LENGTH(replacement);
  IF strLength>0 THEN
  BEGIN
    FOR iCounter:=1 TO strLength DO
    BEGIN
      strPosition := POS(search, str);
      IF (strPosition<>0) THEN
      BEGIN
        str:=TRIM(COPY(str, 1, strPosition-1)+replacement+COPY(str, strPosition+searchLength,strLength));
        strLength := LENGTH(str);
      END
      ELSE str:= TRIM(str);
    END; // end for
  END;
  StrReplace := str;
END;


FUNCTION QuadVal(CONST S : ShortString) : COMP; PASCAL; ASSEMBLER;
  VAR
    Number : COMP;

  ASM
                PUSH    ESI
                PUSH    EDI
                PUSH    EBX
                PUSH    EBP
                XOR     EDX,EDX
                XOR     EAX,EAX
                MOV     ESI,S
                MOVZX   ECX,BYTE PTR [ESI]
                JECXZ   @OUT
        @LOOP:  SHL     EAX,1
                RCL     EDX,1
                MOV     EBX,EAX
                MOV     EBP,EDX
                SHL     EAX,1
                RCL     EDX,1
                SHL     EAX,1
                RCL     EDX,1
                ADD     EAX,EBX
                ADC     EDX,EBP
                INC     ESI
                MOVZX   EBX,BYTE PTR [ESI]
                SUB     BL,'0'
                JC      @ERR
                CMP     BL,9
                JA      @ERR
                ADD     EAX,EBX
                ADC     EDX,0
                LOOP    @LOOP
                JMP     @OUT
        @ERR:   XOR     EDX,EDX
                XOR     EAX,EAX
        @OUT:   POP     EBP
                POP     EBX
                POP     EDI
                POP     ESI
                MOV     DWORD PTR Number,EAX
                MOV     DWORD PTR Number+4,EDX
                FILD    Number
  END;

//===========================================================================



{ Executes a REXX procedure with one argument }
FUNCTION DoCallRexx(const RexxSrc: array of PChar; const AArg: String): Longint;
{$T-}
VAR
  P:          PChar;
  I,J:        Integer;
  Arg:        RxString;
  RexxRetVal: RxString;
  RexxRC:     SmallWord;
  Instore:    array [0..1] of RxString;
  SrcBuf:     array [0..MaxRexxSrcBuffer-1] of Char;
BEGIN
  { By setting the strlength of the output RXSTRING to zero, we   }
  { force the interpreter to allocate memory and return it to us. }
  { We could provide a buffer for the interpreter to use instead. }
//  RexxRetVal.strlength := 0;
  { Create input argument }
  Arg.strlength := Length(AArg);
  Arg.strptr := @AArg[1];
  { Create REXX procedure source code in memory }
  J := 0;
  FOR I := Low(RexxSrc) TO High(RexxSrc) DO
  BEGIN
    P := RexxSrc[I];
    WHILE P^ <> #0 DO
    BEGIN
      SrcBuf[J] := P^;
      Inc(P);
      Inc(J);
    END;
    SrcBuf[J]   := #13;         { Carriage Return }
    SrcBuf[J+1] := #10;         { Line Feed       }
    Inc(J, 2);
  END;
  Instore[0].strlength := J;
  Instore[0].strptr := @SrcBuf;
  Instore[1].strlength := 0;
  Instore[1].strptr := nil;
  { Here we call the interpreter }
  DoCallRexx := RexxStart(1    ,        { Number of arguments        }
                  @Arg         ,        { Argument array             }
                  'VpcCallRexx',        { Name of the REXX procedure }
                  @InStore     ,        { Location of the procedure  }
                  'CMD'        ,        { Initial environment name   }
                  rxCommand    ,        { Code for how invoked       }
                  nil          ,        { No EXITs on this call      }
                  RexxRC       ,        { Rexx program output        }
                  RexxRetVal);          { Rexx program output        }

    //Writeln('REXX RetVal Lenght:',RexxRetVal.strlength); // only for debug;
    //Writeln('REXX RetVal PChar.:',RexxRetVal.strptr);    // only for debug;
    rexxRxString := StrPas(RexxRetVal.strptr); // Value that the REXX code returns by a "RETURN ..." line at the end of the function...
  { Release storage allocated by REXX }
  IF Assigned(RexxRetVal.strptr) THEN DosFreeMem(RexxRetVal.strptr);
  DosFreeMem(Instore[1].strptr);
{$T+}
END;


//==============CPU Information retrieval====================================
FUNCTION CPUID_PRESENT : boolean; assembler; {&Uses ALL}
// procedure has been taken from "AMD Processor Recognition" pdf file (http://www.amd.com)
ASM
 pushfd                 // save flags
    pop  EAX
    mov  EBX, EAX
    xor  EAX, $00200000 // switch $00200000 cell
   push  EAX
  popfd                 // save changed flags
 pushfd
    pop  EAX
    cmp  EAX, EBX
     jz  @NO_CPUID      // are the flags still changed? if not - there's no cpuid
    mov  AL, 1
@NO_CPUID:
END;

FUNCTION CPU_BasicID : string; {&Uses ALL} // [ 12 ];
VAR
   ebxReg, ecxReg, edxReg : array[ 1..4 ] of char;
BEGIN
  ASM
    mov  EAX, $00
  cpuid
    mov  [ebxReg], EBX
    mov  [ecxReg], ECX
    mov  [edxReg], EDX
  END;
  CPU_BasicID:=ebxReg+edxReg+ecxReg;
END;

//function MaxExtCPUID : dword; assembler; // get max. extended info instruction
FUNCTION MaxExtCPUID : Word; assembler; // get max. extended info instruction
ASM
   mov  EAX, $80000000
 cpuid
END;

FUNCTION CPU_ExtID : string; {&Uses ALL} //[ 48 ]; // processor's name
VAR
    eaxReg, ebxReg, ecxReg, edxReg : array[ 1..4 ] of char;
                          TempName : string[ 48 ];
BEGIN
  ASM
    mov  EAX, $80000002
  cpuid
    mov  [eaxReg], EAX
    mov  [ebxReg], EBX
    mov  [ecxReg], ECX
    mov  [edxReg], EDX
  END;
  TempName:=eaxReg+ebxReg+ecxReg+edxReg;
  ASM
    mov  EAX, $80000003
  cpuid
    mov  [eaxReg], EAX
    mov  [ebxReg], EBX
    mov  [ecxReg], ECX
    mov  [edxReg], EDX
  END;
  TempName:=TempName+eaxReg+ebxReg+ecxReg+edxReg;
  ASM
    mov  EAX, $80000004
  cpuid
    mov  [eaxReg], EAX
    mov  [ebxReg], EBX
    mov  [ecxReg], ECX
    mov  [edxReg], EDX
  END;
  CPU_ExtID:=TempName+eaxReg+ebxReg+ecxReg+edxReg;
END;


// ==============Juergen - 2004-07-28 - Intel - direct family, model, stepping ================================
function CPU_Family : Word; assembler; {&Uses eax} // extended features, return in EAX register
asm
    mov  EAX, $01
  cpuid
    shr  EAX, 8
end;
function CPU_Model : Word; assembler; {&Uses eax} // extended features, return in EAX register
asm
    mov  EAX, $01
  cpuid
    shr  EAX, 4
end;
function CPU_Stepping : Word; assembler; {&Uses eax} // extended features, return in EAX register
asm
    mov  EAX, $01
  cpuid
end;
// ======================================================

//----------------------------------------------------
//--            function EAX=1 cpuid                --
//----------------------------------------------------
//function CPU_Feat1EAX : dword; assembler; // extended features, return in EAX register
function CPU_Feat1EAX : Word; assembler; {&Uses eax} // extended features, return in EAX register
asm
    mov  EAX, $01
  cpuid
end;

//function CPU_Feat1EBX : dword; assembler; //  extended features, return in EBX register
function CPU_Feat1EBX : Word; assembler; {&Uses eax, ebx} //  extended features, return in EBX register
asm
    mov  EAX, $01
  cpuid
    mov  EAX, EBX
end;

//function CPU_Feat1EDX : dword; assembler; // EDX register(feature flags)
function CPU_Feat1EDX : Word; assembler; {&Uses eax,edx} // EDX register(feature flags)
asm
    mov  EAX, $01
  cpuid
    mov  EAX, EDX
end;
//--------------------------------------------------- end

function AMDCPU_Generation : byte; assembler; {&Uses eax}
asm
   mov  EAX, $80000001
 cpuid
   and  EAX, $F00
   shr  EAX, 8
   mov  AL, byte ptr [EAX]
end;

//function CPU_ExtFeat : dword; assembler;
function CPU_ExtFeat : Word; assembler; {&Uses eax, edx}
asm
   mov  EAX, $80000001
 cpuid
   mov  EAX, EDX
end;

//function L1InstructionCPU_CacheInfo : dword; assembler; // for AMD only
function L1InstructionCPU_CacheInfo : Word; assembler; {&Uses eax,edx} // for AMD only
asm
   mov  EAX, $80000005
 cpuid
   shr  EDX, $18          // move 24 bits right (bits 31-24 of 0-31 dword)
   mov  EAX, EDX
end;

//function L1DataCPU_CacheInfo : dword; assembler;  // for AMD only
function L1DataCPU_CacheInfo : Word; assembler; {&Uses ALL}  // for AMD only
asm
   mov  EAX, $80000005
 cpuid
   shr  ECX, $18          // move 24 bits right (bits 31-24 of 0-31 dword)
   mov  EAX, ECX
end;

//function L2CPU_CacheInfo : dword; assembler; // for AMD only
function L2CPU_CacheInfo : Word; assembler; {&Uses ALL} // for AMD only
asm
   mov  EAX, $80000006
 cpuid
   shr  ECX, $10          // move 16 bits right (bits 31-16 of 0-31 dword)
   mov  EAX, ECX
end;

FUNCTION IntelIdentify : string; //[ 60 ] was length of 29 before upgrading/correcting IntelName strings;
BEGIN
//  typ:=byte(( CPU_Feat1EAX AND $07000 ) SHR 12 ); // old code
  typ:=byte(( CPU_Feat1EAX AND $03000 ) SHR 12 );   // $03000 = 00110000.00000000 - only bit 12 and 13 needed!!!
  ProcType:=IntelTypes[typ];
  CASE family OF
    $04 : CASE model OF
            $02 : IntelIdentify:=IntelNames[ 1 ];
            $03 : IntelIdentify:=IntelNames[ 2 ]; // the same values for all DX2 (2, 3, 4 types)
            $04 : IntelIdentify:=IntelNames[ 5 ];
            $05 : IntelIdentify:=IntelNames[ 6 ];
            $07 : IntelIdentify:=IntelNames[ 7 ];
            $08 : IF typ <> $00 then IntelIdentify:=IntelNames[ 9 ] else IntelIdentify:=IntelNames[ 8 ];
            else if model < $02 then IntelIdentify:=IntelNames[ 0 ];
          end;
    $05 : case model of
            $01 : if typ = $01 then IntelIdentify:=IntelNames[ 12 ] else IntelIdentify:=IntelNames[ 10 ];
            $02 : if typ = $01 then IntelIdentify:=IntelNames[ 13 ] else IntelIdentify:=IntelNames[ 11 ];
            $03 : IntelIdentify:=IntelNames[ 14 ];
            $04 : if typ = $01 then IntelIdentify:=IntelNames[ 16 ] else IntelIdentify:=IntelNames[ 15 ];
          end;
    $06 : case model of
            $01 : IntelIdentify:=IntelNames[ 17 ];
            $03 : if typ = $01 then IntelIdentify:=IntelNames[ 25 ] else IntelIdentify:=IntelNames[ 18 ];
            $05 : IntelIdentify:=IntelNames[ 19 ];
            $06 : IntelIdentify:=IntelNames[ 20 ];
            $07 : IntelIdentify:=IntelNames[ 21 ];
            $08 : IntelIdentify:=IntelNames[ 22 ];
            $0A : IntelIdentify:=IntelNames[ 23 ];
            $0B : IntelIdentify:=IntelNames[ 24 ];
          end;
    $0F : case model of
            $00 : IntelIdentify:=IntelNames[ 26 ];
            $01 : IntelIdentify:=IntelNames[ 27 ];
            $02 : IntelIdentify:=IntelNames[ 28 ];
          end;
  ELSE IntelIdentify:='model unknown';
  END
END;

PROCEDURE cpuinfo;
BEGIN
  if CPUID_PRESENT = FALSE
    THEN Writeln('CPUID NOT supported!')
    ELSE
    BEGIN
      ProcName:=CPU_BasicID;
      Writeln('ProcName:',ProcName);
      MaxIDcode:=MaxExtCPUID; // check if $8000000x instructions are available
      //---------------------------------------------------------------------------------
      IF ProcName = 'AuthenticAMD' THEN // === AMD ===
      BEGIN
        family   := byte(( CPU_Feat1EAX AND $F00 ) SHR 8 );
        model    := byte(( CPU_Feat1EAX AND $0F0 ) SHR 4 );
        stepping := byte(  CPU_Feat1EAX AND $00F );
        //generation:=byte( AMDCPU_Generation ); // EA ACCESS VIOLATION!!!!
        generation:=0;
        IF (MaxIDcode >= $80000004) THEN
        BEGIN
          ExtProcName:=CPU_ExtID;
          Writeln('ExtProcName=',extProcName);
        END;
        Txt:='Generation:    ' + IntToStr( generation ) + #0;
        Writeln('CPU=',Txt);
        IF MaxIDcode >= $80000004 THEN
        BEGIN // extended information
          DNowcode:=boolean( abs(( CPU_ExtFeat AND $80000000 ) SHR 31 )); // bit 31,  'AMD 3DNow! Extensions: -  BeOrNot( boolean(( CPU_ExtFeat AND $40000000 ) SHR 30 )) );   -- bit 30
          MMXcode:=boolean(( CPU_ExtFeat AND $00800000 ) SHR 23 );        // bit 23  'AMD MMX Extensions: ', BeOrNot( boolean(( CPU_ExtFeat AND $00400000 ) SHR 22 )) );   -- bit 22
        END;                                                              // 'On-chip APIC Hardware: ', BeOrNot( boolean(( CPU_ExtFeat AND $00000200 ) SHR 9 )) );   // bit 9
        IF MaxIDcode >= $80000005 THEN
        BEGIN // L1 cache info
          L1codeCache:=L1InstructionCPU_CacheInfo; // kb
          L1DataCache:=L1InstructionCPU_CacheInfo; // Kb
          Txt:='L1 instruction cache:    ' + IntToStr( L1CodeCache ) + ' KB' + #0;
          Writeln('L1CodeCache=',Txt);
          Txt:='L1 data cache:    ' + IntToStr( L1DataCache ) + ' KB' + #0;
          Writeln('L1DataCache=',Txt);
        END;
        IF MaxIDcode >= $80000006 THEN
        BEGIN
           L2Cache:=L2CPU_CacheInfo; // Kb
           Txt:='L2 cache:    ' + IntToStr( L2Cache ) + ' KB' + #0;
           Writeln('L2=',Txt);
        END;
      END; // end of AMD info
      //---------------------------------------------------------------------------------
      IF ProcName = 'GenuineIntel' // === Intel ===
      THEN
      BEGIN // please note that Intel's cache info is quite complicated, so I was too lazy to include it...
//        family   := byte(( CPU_Feat1EAX AND $F00 ) SHR 8 );  // 8-11 bits of EAX
//        model    := byte(( CPU_Feat1EAX AND $0F0 ) SHR 4 );  // 4-7 bits of EAX
//        stepping := byte(  CPU_Feat1EAX AND $00F );          // 0-3 bits of EAX
        family   := byte(CPU_Family AND $00F);  // 8-11 bits of EAX
        model    := byte(CPU_Model AND $00F);  // 4-7 bits of EAX
        stepping := byte(CPU_Stepping AND $00F);          // 0-3 bits of EAX

//Writeln('family ',family);
//Writeln('model ',model);
//Writeln('stepping ',stepping);
//Writeln('EAX komplett: ',(CPU_Feat1EAX));
//Writeln('EAX komplett: ',byte(CPU_Feat1EAX));
        ExtInfo:='Extended information: ' + IntelIdentify + #0;   //----------- proc. name identify
        Writeln('ExtProcInfo=',ExtInfo);
        Writeln('ProcType=',ProcType);
        IF MaxIDcode >= $80000004 THEN
        BEGIN
          ExtProcName:=CPU_ExtID;
          Writeln('ExtProcName=',ExtProcName);
        END;
        IF boolean(( CPU_Feat1EDX AND $00000200 ) SHR 9 ) = TRUE // bit 9 of EAX
          THEN Txt:='       APIC:   present' + #0
          ELSE Txt:='       APIC:   not available' + #0;
        Writeln('APIC=',Txt);
        MMXcode:=boolean(( CPU_Feat1EDX AND $00800000 ) SHR 23 );
        DNowcode:=FALSE;  // Intel does NOT support 3DNow! instructions for now...   TODO - J.U. - 2004-06-30 - Intel supports 3DNow!
      END; // end of Intel info

      Txt:=IntToStr( family ) + #0;
      Writeln('Family=',Txt);
      Txt:=IntToStr( model ) + #0;
      Writeln('Model=',Txt);
      Txt:=IntToStr( stepping ) + #0;
      Writeln('Stepping=',Txt);

      if MMXcode  = TRUE THEN Txt:='Intel MMX' + #0;
      if DNowcode = TRUE THEN Txt:='AMD 3D Now!' + #0;
      if ( MMXcode = TRUE ) AND ( DNowCode = TRUE ) THEN Txt:='AMD 3D Now! and Intel MMX' + #0;
      Writeln('Extension=',Txt);
    END;
//    Txt:=IntToStr( GetProcessorSpeed );
//    Txt:=Txt + ' MHz' + #0;
//    Writeln('ProcessorSpeed=',Txt);
END;
//===========================================================================


//==============Processor Speed==============================================
FUNCTION getCPUTicks: TQWord; assembler; {&frame-} {&uses eax,ecx,edx,esp}
//see also: http://www.geocities.com/izenkov/howto-rdtsc.htm
ASM
  push   0
  push   0
  mov    ecx,esp
  rdtsc
  mov    [ecx+4],edx
  mov    [ecx],eax
  fild   qword ptr [ecx]
  pop    eax
  pop    eax
END;



FUNCTION getProcessorSpeed:Extended;

(** Uses RDTSC Command to get the Processor Timer count **)
VAR
  Buf  :packed array[0..0] of longint;
  lintTickOld   : longint;
  cpuTicksStart, cpuTicksStop : TQWord;
BEGIN
  Try
    SysCtrlEnterCritSec; //DosEnterCritSec;

    cpuTicksStart := getCPUTicks;
    DosQuerySysInfo(14, 14, Buf[0], sizeof(Buf[0]));
    lintTickOld := Buf[0];

    WHILE Buf[0] < (lintTickOld + 100) DO
      DosQuerySysInfo(14, 14, Buf[0], sizeof(Buf[0]));
    cpuTicksStop := getCPUTicks;

    SysCtrlLeaveCritSec; // DosExitCritSec;
  Except
  END;
  Result := (cpuTicksStop-cpuTicksStart)/((Buf[0]-lintTickOld)*1000);
  IF Result<0      then Result:= 0.0;
END;
//===========================================================================


//==============Bootdrive====================================================
FUNCTION getBootDrive : String; //bootdrive as "C:", "D:",...
VAR
  Buf : packed array[0..0] of longint;
BEGIN
  IF DosQuerySysInfo (5, 5, Buf, sizeof (Buf)) = 0 THEN
    getBootDrive := chr(64+Buf[0])+':'
  ELSE getBootDrive := 'C:';
END; // end getBootDrive
//===========================================================================




//==============Uptime=======================================================
FUNCTION CPU_UPTIME: LongInt;  // was called: 'uptimer_cpu_seconds'
VAR
  tmrfreq, rc: LongInt;
  tmrtime: qword;
BEGIN
  rc:=dostmrqueryfreq(tmrfreq);
  IF (rc<>0) THEN
  BEGIN // no hi_res timer available?
    CPU_UPTIME:=syssysmscount DIV 1000;
    exit;
  END;
  dostmrquerytime(tmrtime);
  CPU_UPTIME:=round((4294967296.0*tmrtime.hi+double2quad(tmrtime.lo)) / tmrfreq);
END;

// Alternative uptime function...from System (beware of the overflow after 49.7 days!!!!!)
FUNCTION SYS_UPTIME : LongInt;
VAR
    lnmsuptime, lnHelp : LongInt;
    lnDays, lnHours, lnMinutes, lnSeconds  : ShortInt;
BEGIN
   sys_uptime := SysSysMsCount DIV 1000;   // Number of ms since last boot
END;


FUNCTION uptimerstr(ut: longint): string;
BEGIN
{$R-}
  uptimerstr:=leadingzero(umod(udiv(ut, 3600*24), 365))+'d '+
  leadingzero(umod(udiv(ut, 3600), 24))+'h '+
  leadingzero(umod(udiv(ut, 60), 60))+'m '+
  leadingzero(umod(ut, 60))+'s';
{$R+}
END;

FUNCTION ttuptimerstr(ut: TTimestamp): string;
BEGIN
{$R-}
  ttuptimerstr:=leadingzero(ut.Date)+'d '+
  leadingzero(umod(udiv(ut.Time, 3600000), 24))+'h '+
  leadingzero(umod(udiv(ut.Time, 60000), 60))+'m '+
  leadingzero(umod(udiv(ut.Time, 1000), 60))+'s';
{$R+}
END;


FUNCTION Timestamp2LongInt(ut: TTimestamp): LongInt;
VAR
  high, days, time : Cardinal;
BEGIN
{$R-}
//  Writeln(ut.Date);
//  Writeln(ut.Time);
    days := umul(ut.Date, 86400, @high);
//    Writeln(days);
  IF (ut.Date>0) THEN
    time := udiv(ut.Time, 10000)
  ELSE
    time := udiv(ut.Time, 1000);

  //Writeln(uptimerstr(days+time));
  Timestamp2LongInt := days+time;
  (*
  (umul(ut.Date, 3600000*24, @high)+
    umul(ut.Time, 3600000*24, @high)+
    umul(ut.Time, 60000*60, @high)+
    umul(ut.Time, 1000*60, @high));
  *)
{$R+}
END;


PROCEDURE SWAPPER_UPTIME;
Var
  hdirFindHandle  : hDir;
  FindBuffer      : FileFindBuf3; // Returned from FindFirst/Next
  ulResultBufLen  : ULong;
  ulFindCount     : ULong;        // Look for 1 file at a time
  rc              : ApiRet;       // Return code
  dtFile, dtNow, dtDifference   : DateTime;
  tdtFile, tdtNow : TDateTime;

  tdtDifference   : TDateTime;
  difTimestamp, datenowTimestamp, fileTimestamp : TTimeStamp;
  UptimeDays, UptimeSeconds : Integer;

  sm:array[0..260] of char;
BEGIN
  hdirFindHandle := hDir_Create;
  ulResultBufLen := sizeof(FileFindBuf3);
  ulFindCount    := 1;

  StrPCopy(sm, getBootDrive+'\OS2\SYSTEM\SWAPPER.DAT');

  rc := DosFindFirst(
    sm,              // File pattern - all files
    hdirFindHandle,  // Directory search handle
    file_Normal,     // Search attribute
    FindBuffer,      // Result buffer
    ulResultBufLen,  // Result buffer length
    ulFindCount,     // Number of entries to find
    fil_Standard);   // Return level 1 file info

  IF rc <> No_Error THEN
  BEGIN
    Writeln('DosFindFirst error: return code = ',rc);
    //Halt(1);
  END;

  WITH FindBuffer DO
  BEGIN
    UnpackTime(FindBuffer.fdateCreation shl 16
               +FindBuffer.ftimeCreation,
               dtFile);
    tdtFile := FileDateToDateTime(FindBuffer.fdateCreation shl 16+FindBuffer.ftimeCreation); // FileDate to TDateTime
    tdtNow := now;
    FileTimestamp := DateTimeToTimeStamp(tdtFile);
    DateNowTimestamp := DateTimeToTimeStamp(tdtNow);
    DifTimestamp.Date := DateNowTimestamp.Date-FileTimestamp.Date;
    DifTimestamp.Time := DateNowTimestamp.Time-FileTimestamp.Time;
    Writeln('SystemUptime3:',ttuptimerstr(DifTimestamp)+crlf);
    //Writeln('Timestamp2LongInt :',Timestamp2LongInt(DifTimestamp));
  END; // end FindBuffer
END;


FUNCTION uptime: String;
VAR
  tmp_uptime_sys,
  tmp_uptime_cpu,
  tmp_uptime_swap : LongInt;
BEGIN
  //tmp_uptime_sys := SYS_UPTIME;
  //tmp_uptime_cpu := CPU_UPTIME;
  //Writeln('SYS_UPTIME (LongInt): ',SYS_UPTIME);
  //Writeln('CPU_UPTIME (LongInt): ',CPU_UPTIME);
  SWAPPER_UPTIME;

  // Here I want to test the different uptime results and return the most logical value...
  // This will be finished later...for now we print all three values!!!
  //  Writeln('SWAPPER_UPTIME (LongInt): ',SWAPPER_UPTIME);
  (*
  tmp_uptime_swap:=
  IF (tmp_uptime_sys>(tmp_uptime_cpu+xxx) OR tmp_uptime_sys<(tmp_uptime_cpu-xxx)) THEN
  BEGIN
  END
  ELSE
  IF
  *)
  //uptime:='...';
END;


//===========================================================================


//==============DriveList====================================================
FUNCTION getDriveList: AnsiString;
VAR
  drives : DriveSet;
  drive  : Char;
  tmpStr    : AnsiString;
  driveType : tDriveType;
  driveTypeStr : String;
  diskSizeQuad, diskFreeQuad : TQuad;

BEGIN
  tmpStr := '';
  GetValidDrives(drives);
  FOR Drive := 'C' TO 'Z' DO
  BEGIN
    driveType := GetDriveType(Drive);
    IF (driveType = dtHDHPFS)         THEN driveTypeStr := 'HPFS'
    ELSE IF (driveType = dtJFS)       THEN driveTypeStr := 'JFS'
    ELSE IF (driveType = dtHDFAT)     THEN driveTypeStr := 'FAT'
    ELSE IF (driveType = dtHDNTFS)    THEN driveTypeStr := 'NTFS'
    ELSE IF (driveType = dtTVFS)      THEN driveTypeStr := 'TVFS'
    ELSE IF (driveType = dtHDExt2)    THEN driveTypeStr := 'EXT2'
    ELSE IF (driveType = dtLAN)       THEN driveTypeStr := 'LAN'
    ELSE IF (driveType = dtNovellNet) THEN driveTypeStr := 'NOVELL'
    ELSE IF (driveType = dtCDRom)     THEN driveTypeStr := 'CD-ROM'
    ELSE driveTypeStr := 'UNKNOWN';

    IF (POS(driveTypeStr, 'HPFS;JFS;FAT;NTFS;TVFS;EXT2;LAN;NOVELL;CD-ROM;')<>0) THEN
      tmpStr := tmpStr+'Partition:'+Drive+' Mount:/ Type:'+driveTypeStr+
                       ' PartSize:'+squad2str(SysDiskSizeLong(ORD(Drive)-64)/1024.0)+'KB'+
                       ' PartAvail:'+squad2str(SysDiskFreeLong(ORD(Drive)-64)/1024.0)+'KB'+crlf;
  END; // end FOR 'C' TO 'Z' DO...
  getDriveList := tmpStr;
END;
//===========================================================================


//==============SWAPPER.DAT==================================================
FUNCTION getSwapFileSizeByDir : String;
VAR
  tr  : TRedirExec;
BEGIN
  tr := TRedirExec.Create;                   // Create a TRedirExec instance
  IF Assigned( tr ) THEN                     // If creation was ok...
    try                                      // Catch any errors
      { Execute the command to grab the output from }
      tr.Execute( 'CMD.EXE', '/C DIR C:\OS2\SYSTEM\SWAPPER.DAT', nil );
      WHILE NOT tr.Terminated DO         // While command is executing
      IF tr.MessageReady THEN          // Ask if a line is ready
        Writeln( tr.Message )          // - Display it
      ELSE
        DosSleep( 30 );                // - otherwise wait a little
    finally
      tr.Destroy;                            // Free the instance
    getSwapFileSizeByDir := '2097152'; // default (2048*1024)
  END
  ELSE getSwapFileSizeByDir := '2097152';
END;



FUNCTION getSwapFileSizeByRexx : String;
{$I-}
VAR
  Buf  :packed array[0..0] of longint;
  swapFileSize : longint;
  f : File;
  RC : LONGINT;
CONST
  defaultSwapFileSize = (1024*2048);
  getSwapSizeByRexx: array[0..6] of PChar =
                    ('Call Rxfuncadd "SysLoadFuncs","Rexxutil","SysLoadFuncs"',
                     'Call SysLoadFuncs',
                     'parse arg sBootDrive',
                     'sFile = sBootDrive":\OS2\SYSTEM\SWAPPER.DAT"',
                     'call SysFileTree sFile, aRes, "F"',
                     'parse var aRes.1 sTime sDate sSize sRest',
                     'RETURN sSize'
                     );
BEGIN
  swapFileSize := defaultSwapFileSize; // default size
  IF DosQuerySysInfo ( 5,  5, Buf[0], sizeof(Buf[0])) = 0 THEN
  BEGIN
     IF FileExists(chr(64+Buf[0])+':\OS2\SYSTEM\SWAPPER.DAT') THEN
     BEGIN
       assign(f, chr(64+Buf[0])+':\OS2\SYSTEM\SWAPPER.DAT');
       FileMode := open_access_ReadOnly + open_share_DenyNone;
       reset(f,1);
       swapFileSize := fileSize(f);
       {$I-}
       close(f);
       {$I+}
       IF IOResult <> 0 THEN // filesize could not be received with low level fileSize function...try REXX instead...
       BEGIN
         RC := DoCallRexx(getSwapSizeByRexx, chr(64+Buf[0]));
         IF RC <> 0 then swapFileSize := defaultSwapFileSize // default size
         ELSE
         BEGIN
           try
             swapFileSize := strtoint(rexxRxString);
           except // ooops - returned RexxString couldn't be converted to integer
             swapFileSize := defaultSwapFileSize; // set default instead
           end;
         END;
       END; // end of REXX file size code
     END // try to get filesize of swapper.dat by low level fileSize function.
     ELSE swapFileSize := defaultSwapFileSize; // default size
  END;
  getSwapFileSizeByRexx := 'TotalAvailSwap:'+squad2str(SysDiskFreeLong(Buf[0])/1024.0)+'KB'+crlf+'TotalUsedSwap:'+ulong2str(swapFileSize div 1024)+'KB'+crlf;
{$I+}
END;
//===========================================================================



//====================MEMORY=================================================
FUNCTION getMemory: string;
VAR
  Buf          : packed array[0..4] of longint;
  swapFileSize : longint;
  f            : File;
BEGIN
  IF DosQuerySysInfo (17, 21, Buf, sizeof(Buf)) = 0 THEN
  getMemory := 'TotalPhysicalMem:'+ulong2str(Buf[0] DIV 1024)+'KB'+crlf+'TotalAvailMem:'+ulong2str(WrapDos16MemAvail DIV 1024)+'KB'+crlf;

  //Writeln(Buf[0] div 1024);  // 17 - QSV_TOTPHYSMEM (Total number of bytes of physical memory in the system)
  //Writeln(Buf[1] div 1024);  // 18 - QSV_TOTRESMEM (Total number of bytes of resident memory in the system)
  //Writeln(Buf[2] div 1024);  // 19 - QSV_TOTAVAILMEM (Max. number of bytes of memory that can be allocated by all processes in the system)
  //Writeln(Buf[3] div 1024);  // 20 - QSV_MAXPRMEM (Max. number of bytes of memory that this process can allocate in its private arena)
  //Writeln(Buf[4] div 1024);  // 21 - QSV_MAXSHMEM (Max. number of bytes of memory that a process can allocate in the shared arena)
END;
//===========================================================================



//==============Number of Processors=========================================
FUNCTION getNumberOfProcessors: string;
VAR
  Buf : packed array [0..0] of longint;
BEGIN
  IF DosQuerySysInfo (26, 26, Buf, sizeof(Buf)) = 0 THEN
    getNumberOfProcessors := ulong2str(Buf[0])+crlf
  ELSE getNumberOfProcessors := '1'+crlf;
END;
//===========================================================================



//==============OS/2 and eCS version=========================================
function getOsVersion: string;
var
  Running_OS_Name : string;
  Buf : packed array [5..12] of longint;
  Sgn : string;
  f  : file;
  fp : longint;
  sp : longint;
  p1, p2 : integer;
  kernelAddon:String;
  vls   : String[2];
  osVer : Word;
BEGIN
  osVer := OSVersion;
  Running_OS_Name   := 'OS/2 v'+inttostr(lo(osVer))+'.'+inttostr(hi(osVer));
  if DosQuerySysInfo (5, 12, Buf, sizeof (Buf)) = 0 then
  begin
    FileMode := open_access_ReadOnly + open_share_DenyNone;
    assign (f, chr (64 + Buf [5]) + ':\OS2KRNL');
    reset (f, 1);
    seek (f, $3C);
    blockread (f, fp, 4);
    seek (f, fp+$88);
    blockread (f, fp, 4);
    seek (f, fp);
    blockread (f, Sgn [0], 1);
    blockread (f, Sgn [1], length (Sgn));
    p1 := pos ('@#', Sgn);
    p2 := pos ('#@', Sgn);
    if (IoResult = 0) and (p1 <> 0) and (p2 <> 0) and (p2 > (p1+2)) then
    begin
      kernelAddon := trim(copy(Sgn, p2+2,4)); // _W4, _W3, _UNI, _SMP after the '#@' Block!!!
      Sgn := copy (Sgn, p1+2, p2-p1-2);
      Sgn := Sgn+kernelAddon;
      p1 := pos (':', Sgn);

      if p1 <> 0 then Sgn := copy (Sgn, p1+1, 255);

      Running_OS_Name := Running_OS_Name + crlf + 'KernelVersion:' + Sgn;
    end
    else
    begin
      Buf [11] := Buf [11] div 10;
      if (Buf [11] = 2) and (Buf [12] >= 30) and (Buf [12] < 90) then
      begin
        Buf [11] := Buf [12] div 10;
        Buf [12] := Buf [12] mod 10;
      end;
      //str (Buf[11], vhs);
      str (Buf[12], vls);
      if length (vls) = 1 then vls := '0' + vls;
      if vls [length (vls)] = '0' then dec (vls [0]);
      //Running_OS_Name := Running_OS_Name + vhs + '.' + vls  ;
      Running_OS_Name := Running_OS_Name + ulong2str(Buf[11]) + '.' + vls  ;
    end;
    close (f);
    if IoResult <> 0 then;
    getOsVersion := Running_OS_Name;
  end;
END;


FUNCTION getOsMainVersion : String; // is it eComStation from Serenity or OS/2 from IBM?
BEGIN
  IF FileExists(getBootDrive+'\ECS\INSTALL\SYSLEVEL.ECS') OR FileExists(getBootDrive+'\ECS\INSTALL\eCScd.bmp') THEN // check eCS/eComStation Syslevel file...
  BEGIN
    getOsMainVersion := 'eComStation'
  END
  ELSE IF FileExists(getBootDrive+'\OS2\INSTALL\SYSLEVEL.OS2') THEN // check OS/2 Syslevel file...
  BEGIN
    getOsMainVersion := 'OS/2'
  END
  ELSE getOsMainVersion := 'unknown';
END;

FUNCTION getServiceLevel : String; //....unfinished....
BEGIN
END;
//===========================================================================



//==============OEMHELP======================================================
{$IfDef OS2}
VAR
  oemhlp_failed : boolean;
  oemhlp_handle : longint=-1;
  resmgr_failed : boolean;
  resmgr_handle : longint=-1;

PROCEDURE open_oemhlp;
BEGIN
  IF SysFileOpen('OEMHLP$',open_access_readonly+open_share_denynone,oemhlp_handle)<>0 THEN
  BEGIN
    oemhlp_handle:=-1;
    oemhlp_failed:=true;
  END
  ELSE oemhlp_failed:=false;
END;

PROCEDURE close_oemhlp;
BEGIN
    SysFileClose(oemhlp_handle);
END;

PROCEDURE open_resmgr;
BEGIN
  IF SysFileOpen('RESMGR$',open_access_readonly+open_share_denynone,resmgr_handle)<>0 THEN
  BEGIN
    resmgr_handle:=-1;
    resmgr_failed:=true;
  END
  ELSE resmgr_failed:=false;
END;

PROCEDURE close_resmgr;
BEGIN
    SysFileClose(oemhlp_handle);
END;

{$EndIf OS2}
//===========================================================================







//==============Harddrives===================================================
(*
Adapter: IDE_0 (E)IDE Controller
Device Type: MS-IDE         Bus/Width: PCI 16 BIT
IRQ Level = 14  PCI Pin = NONE  Flg = EXCLUSIVE
I/O = 0X01F0  Len =   8  Flg = EXCLUSIVE   Addr Lines = 16
I/O = 0X03F6  Len =   1  Flg = EXCLUSIVE   Addr Lines = 16
I/O = 0XFC90  Len =   8  Flg = EXCLUSIVE   Addr Lines = 16

  Device: HD_0 IBM-DJSA-210                            FIXED  DISK


Adapter: USB Mass Storage Device Class driver
...
*)
PROCEDURE getPhyDriveList;
VAR
  tr  : TRedirExec;
  eideType, scsiType : Boolean;
  adptrFound         : Boolean;
  line               : String;
BEGIN
  tr := TRedirExec.Create;                   // Create a TRedirExec instance
  IF Assigned(tr) THEN                       // If creation was ok...
  TRY                                        // Catch any errors
    adptrFound:=false;
    eideType  := false;
    scsiType  := false;
    { Execute the command to grab the output from }
    tr.Execute('RMVIEW.EXE', '', nil);
    WHILE NOT tr.Terminated DO               // While command is executing
      IF tr.MessageReady THEN                // Ask if a line is ready
      BEGIN
        line := tr.Message;
        IF (NOT adptrFound) THEN
//          IF ((POS('Adapter: IDE', line)<>0) OR
//             ((POS('Adapter: ', line)<>0) AND (POS('SCSI', line)<>0)) OR
          IF ((POS('Device Type: MS-IDE', line)<>0) OR
              (POS('Device Type: MS-SCSI', line)<>0) ) THEN
          BEGIN // found controller...devices follow
            eideType  := false;
            scsiType  := false;
            IF POS('Device Type: MS-IDE', line)<>0 THEN eideType := true
            ELSE IF POS('Device Type: MS-SCSI', line)<>0 THEN scsiType := true;

            WHILE NOT tr.Terminated DO
            BEGIN
              IF tr.MessageReady THEN
              BEGIN
                line := tr.Message;
                IF POS('Adapter: ', line)<>0 THEN
                  BREAK
                ELSE
                  IF POS('Device: ', line)<>0 THEN
                  BEGIN
                    line := COPY(line, POS('Device: ', line)+8,80);
                    line := StrReplace(line, '  ',' ');
                    IF eideType THEN Writeln('EIDE: ',line)
                    ELSE IF scsiType THEN Writeln('SCSI: ',line)
                    ELSE Writeln('OTHER: ',line);
                  END;
              END
              ELSE DosSleep(30);
            END; // end while
          END;
      END // end if - first IF tr.MessageReady...BEGIN..END
      ELSE DosSleep(30);                     // - otherwise wait a little
  FINALLY
    tr.Destroy;                              // Free the instance
  END // end try...finally
  ELSE // error in try-part
    Writeln( 'Error creating TRedirExec class instance' );
  Writeln;
END;
//===========================================================================




//==============Network======================================================
PROCEDURE getNetworkInfo;
VAR
  tr                              : TRedirExec;
  networkInterfaces               : Array[1..10] of String[80];
  kbytesReceived, kbytesSent      : ULong;
  interfaceCounter, outputCounter : Byte;
  line                            : String;
  tmpString : String;
BEGIN
  interfaceCounter:=0;
  tr := TRedirExec.Create;                   // Create a TRedirExec instance
  IF Assigned(tr) THEN                       // If creation was ok...
  TRY                                        // Catch any errors
    { Execute the command to grab the output from }
    tr.Execute('NETSTAT.EXE', '-n', nil);
    WHILE NOT tr.Terminated DO               // While command is executing
      IF tr.MessageReady THEN                // Ask if a line is ready
      BEGIN
        line := tr.Message;
        IF POS('Interface ', line)<>0               THEN line := UpperCase(StrReplace(line, 'Interface ', ''));
        IF POS('LOOPBACK', line)<>0                 THEN line := 'loopback'
        ELSE IF POS('SERIAL INTERFACE SL', line)<>0 THEN line := TRIM('serial sl'+COPY(line, POS('SERIAL INTERFACE SL', line)+19, 80))
        ELSE line:=TRIM('lan'+COPY(line,1,2));
        inc(interfaceCounter);
        networkInterfaces[interfaceCounter]:=line;

        // read the following lines....
        WHILE NOT tr.Terminated DO
        BEGIN
          IF tr.MessageReady THEN
          BEGIN
            line := tr.Message;
            IF POS('packets received in unsupported protocols',line)=0 THEN
            BEGIN
              IF POS('total bytes received', line)<>0 THEN
              BEGIN
                tmpString := StrReplace(line, 'total bytes received','');
                networkInterfaces[interfaceCounter]:=networkInterfaces[interfaceCounter]+' received:'+FloatToStr(INT(QuadVal(tmpString)/1024.0))+'KB';
              END
              ELSE IF POS('total bytes sent', line)<>0 THEN
              BEGIN
                tmpString := StrReplace(line, 'total bytes sent','');
                networkInterfaces[interfaceCounter]:=networkInterfaces[interfaceCounter]+' sent:'+FloatToStr(INT(QuadVal(tmpString)/1024.0))+'KB';
              END
            END
            ELSE BREAK;
          END;
        END; // end while
      END // end if - first IF tr.MessageReady...BEGIN..END
      ELSE DosSleep(30);                     // - otherwise wait a little
  FINALLY
    tr.Destroy;                              // Free the instance
  END // end try...finally
  ELSE // error in try-part
    Writeln( 'Error creating TRedirExec class instance' );
  FOR outputCounter:=1 TO interfaceCounter DO
  BEGIN
    IF LENGTH(networkInterfaces[outputCounter])<>0 THEN Writeln(networkInterfaces[outputCounter]);
  END;
  Writeln;
END;
//===========================================================================

//==============PCI-Devicelist===============================================
PROCEDURE getPCIShortList;
BEGIN
  Writeln('PCI-Devicelist:');
  Writeln('...under construction...');
  Writeln;
END;
//===========================================================================



PROCEDURE usageScreen;
BEGIN
 ClrScr;
 Writeln;
 Writeln('  OS2INFO v0.41 - 2006-06-01 - FREEWARE (GPL License)');
 Writeln('  ===================================================');
 Writeln('  Author: Juergen Ulbts (Germany)');
 Writeln('          http://www.juergen-ulbts.de/');
 Writeln('          http://www.jmdb.de/');
 Writeln;
 Writeln('  Many thanks go to the following individuals:');
 Writeln;
 Writeln('       Veit Kannegieser: Author of PCI, MemDisk, OS2CSM,...');
 Writeln('                         Module: Dos16MemAvail call using a wrapper');
 Writeln('                         http://www.kannegieser.net/veit/');
 Writeln('       Dink............: Author of z!, web/2,...');
 Writeln('                         Module: uptimer_cpu_seconds code');
 Writeln('                         http://www.dink.org/');
 Writeln('       Keld R. Hansen..: Author of serveral utilities');
 Writeln('                         Module: CompVal code');
 Writeln('                         http://www.heartware.dk/');
 Writeln('       Konrad Olejnik..: ShowPC for TMT Pascal (C)2003');
 Writeln('                         Module: CPUID codebase');
 Writeln('                         http://wipos.p.lodz.pl/paper/tmtpl/');
 Writeln;
 Writeln;
 Writeln('  Press ENTER for the usage information!');
 Readln;
 ClrScr;
 Writeln;
 Writeln('  This program accepts the following parameters:');
 Writeln('  -? or --help : This usage and info screen');
 Writeln('  <none>       : output of all available information this program offers');
 Writeln('  uptime       : shows the uptime of the machine...I hope it works...');
 Writeln('                 1st: dinks routine (to get around the overflow from 2nd)');
 Writeln('                 2nd: OS/2 build in uptime routine (overflow at 49.7 days)');
 Writeln('                 3rd: SWAPPER.DAT creation DateTime (see note on swap param)');
 Writeln('  osversion    : shows the OS, Version and Kernelversion');
 Writeln('  cpuinfo      : show some information about the processor(s) in the system');
 Writeln('                 This feature is limited. Do not expect 100% accuracy.');
 Writeln('  processors   : shows the number of processors in the system');
 Writeln('  memory       : shows the total amount of memory, the used and the');
 Writeln('                 available memory (avail mem is not correct-needs fix Theseus?) ');
 Writeln('  swap         : shows swapfile size and max space available for it on');
 Writeln('                 the BOOTDRIVE');
 Writeln('                 It ONLY looks for <bootdrive>\OS2\SYSTEM\SWAPPER.DAT');
 Writeln('                 and NO other location!');
 Writeln('  drives       : shows a list of drives with FAT, HPFS and JFS filesystem.');
 Writeln('                 The drive size, available freespace and used space are');
 Writeln('                 also shown.');
 Writeln;
 Writeln('  Press ENTER for the usage information!');
 Readln;
 ClrScr;
 Writeln;
 Writeln('  phydrives    : shows list of physical drives (EIDE/SCSI/OTHER)');
// Writeln('  pci          : shows short list of pci devices (chipset, graphic card,');
// Writeln('                 network cards, harddisc controller)');
 Writeln('  network      : shows list of active network interfaces + loopback');
 Writeln('                 This includes the traffic & connections on those interfaces');

 Writeln;
 Writeln('  All parameters can be combined! I you call "OS2INFO uptime osversion"');
 Writeln('  the program shows the uptime and the osversion information, but not');
 Writeln('  the other information.');

END;


// MAIN PROGRAM
BEGIN
  IF ParamCount = 0 THEN
  BEGIN
    writeln('SystemUptime1:'+uptimerstr(cpu_uptime));
    writeln('SystemUptime2:'+uptimerstr(sys_uptime));
    uptime; // test of final version...

    writeln('SystemVersion:'+getOsVersion);
    writeln('SystemMainVersion:'+getOsMainVersion+crlf);

    writeln('SystemProcessors:'+getNumberOfProcessors);
    writeln('SystemMemory:'+crlf+getMemory);
    writeln('SystemSwap:'+crlf+getSwapFileSizeByRexx);
    writeln('SystemDriveList:'+crlf+getDriveList);

    writeln('SystemPhysicalDriveList:');
    getPhyDriveList;

    writeln('SystemNetworkInfo:');
    getNetworkInfo;

//    getPCIShortList;    // may print the PCI List in the future...

    CPUINFO;
    writeln('CPUSpeed: ~',FormatFloat('####',getProcessorSpeed),' MHz');

    //writeln('SystemCPUID:'+cpuid);
//    writeln;
//    writeln('SPEICHERTEST');
//    Writeln('total available Memory (all processes):',4.0*(SysInfo[qsv_TotAvailMem] Shr 2):1:0,'  (',SysInfo[qsv_TotAvailMem],')');
//    Writeln('available private mem for calling program:',SysInfo[qsv_MaxPrMem]);
//   writeln('SysMemAvail:',SysMemAvail);

  END
  ELSE // CHECK PARAMETERS
  FOR intCounter := 1 to ParamCount do
    BEGIN
      ParamStrI:=UpperCase(ParamStr(intCounter));
      IF (ParamStrI = '-?') OR (ParamStrI = '--HELP') THEN
        usageScreen
      ELSE IF ParamStrI = 'UPTIME' THEN
        BEGIN
          writeln('SystemUptime1:'+uptimerstr(cpu_uptime));
          writeln('SystemUptime2:'+uptimerstr(sys_uptime));
          writeln(uptime); // test of final version... (calls SWAPPER_UPTIME())
        END
      ELSE IF ParamStrI = 'OSVERSION' THEN
        BEGIN
          writeln('SystemVersion:'+getOsVersion);
          writeln('SystemMainVersion:'+getOsMainVersion+crlf);
        END
      ELSE IF ParamStrI = 'CPUINFO' THEN
        BEGIN
          CPUINFO;
          Writeln('CPUSpeed: ~',FormatFloat('####',getProcessorSpeed),' MHz');
        END
      ELSE IF ParamStrI = 'PROCESSORS' THEN
        writeln('SystemProcessors:'+getNumberOfProcessors)
      ELSE IF ParamStrI = 'MEMORY' THEN
        writeln('SystemMemory:'+crlf+getMemory)
      ELSE IF ParamStrI = 'SWAP' THEN
        writeln('SystemSwap:'+crlf+getSwapFileSizeByRexx)
      ELSE IF ParamStrI = 'DRIVES' THEN
        writeln('SystemDriveList:'+crlf+getDriveList)
      ELSE IF ParamStrI = 'PHYDRIVES' THEN
        BEGIN
          writeln('SystemPhysicalDriveList:');
          getPhyDriveList;
        END
      ELSE IF ParamStrI = 'NETWORK' THEN
        BEGIN
          writeln('SystemNetworkInfo:');
          getNetworkInfo;
        END
//      ELSE IF ParamStrI = 'PCI' THEN
//        BEGIN
//          getPCIShortList;
//        END
      ELSE usageScreen;
    END;
END.

