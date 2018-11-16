library dsmodel110;
  {-Berekening van de natuurlijke grondwateraanvulling (=Neerslag - gewasverdamping)
    op basis van:
    - landgebruik
    - gewasfactoren
    - neerslag
    - referentie verdamping

  M.b.t. de gewasfactoren wordt onderscheid gemaakt tussen een f- factor en een
  g-factor.

  Als de verdampingsintensiteit in de zomer evenredig is aan de referentie verdamping,
  dan moet u kiezen voor de f-factor voor de berekening van de natuurlijke grondwater-
  aanvulling. In formule:

  NN = Pr - Ev * f          (1)

  NN: Natuurlijke grondwateraanvulling (m/d)
  Pr: Neerslag (m/d)
  Ev: Referentie gewasverdamping (m/d)
  f: f-factor (-)

  Uit (1) blijkt dat een negatieve natuurlijke grondwateraanvulling ontstaat als
  de term Ev x f groter is dan Pr. De f-factor is afhankelijk van gewas
  (landgebruik) en het seizoen (ref. [...]).

  Bij diepe grondwaterstanden vindt er geen capilaire nalevering meer plaats vanuit
  grondwater naar de wortelzone. Dit is het geval als de grondwaterstand dieper is
  dan de zgn. kritiek z-diepte. De kritieke z-diepte is afhankelijk van het bodemtype
  (ref. [...]). In deze situatie is de gewasverdamping niet meer afhankelijk van de
  referentie verdamping, maar slechts van de neerslag. In formule:

  NN = Pr * g

  NN: Natuurlijke grondwateraanvulling (m/d)
  Pr: Neerslag (m/d)
  g: g-factor

  Ook de grootte van de g-factor is afhankelijk van het gewas (landgebruik) en
  het seizone ([...]).

  Als vuistregel kan worden gesteld dat in relatief droge gebieden (zeg Gt >= VI)
  het gebruik van een g-factor kan worden overwogen (i.p.v. een f-factor) voor de
  berekening van het natuurlijke neerslagoverschot.

  C.H. van Immerzeel, 10-11-2005

  }

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{.$define test}

uses
  ShareMem,
  windows, SysUtils, Classes, LargeArrays,
  ExtParU, USpeedProc, uDCfunc, UdsModel, UdsModelS, xyTable, DUtils, uError {$ifdef test} , Dialogs {$endif};

Const
  cModelID      = 110;  {-Uniek modelnummer}

  {-Beschrijving van de array met afhankelijke variabelen}
  cNrOfDepVar   = 1;    {-Lengte van de array met afhankelijke variabelen}
  cy1           = 1;    {-Natuurlijk neerslagoverschot (m/d)}

  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;

  {-Variabelen die samenhangen met het aanroepen van het model vanuit de Shell}
  cnRP    = 4;   {-Aantal RP-tijdreeksen die door de Shell moeten worden aangeleverd (in
                   de externe parameter Array EP (element EP[ indx-1 ]))}
  cnSQ    = 0;   {-Idem punt-tijdreeksen}
  cnRQ    = 0;   {-Idem lijn-tijdreeksen}

  {-Beschrijving van het eerste element van de externe parameter-array (EP[cEP0])}
  cNrXIndepTblsInEP0 = 5;  {-Aantal XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;  {-Aantal Xdep-tables   in EP[cEP0]}
  {-Nummering van de xIndep-tabellen in EP[cEP0]. De nummers 0&1 zijn gereserveerd}
  cTb_MinMaxValKeys   = 2;
  cTb_Gewasfactoren   = 3;
  cTb_ForGfactor      = 4;
 
  {-Beschrijving van het tweede element van de externe parameter-array (EP[cEP1])}
  {-Opmerking: table 0 van de xIndep-tabellen is gereserveerd}
  {-Nummering van de xdep-tabellen in EP[cEP1]}
  cTb_Pr          = 0;        {-RP1 (Pr): Neerslag (m/d)}
  cTb_Ev          = 1;        {-RP2 (Ev): Referentie gewasverdamping (m/d)}
  cTb_Landgebruik = 2;        {-RP3 (Landgebruik): Landgebruik (-, zie 'EFAC.XLS')}
  cTb_MaandNr     = 3;        {-RP4 (MaandNr): Maandnummer (1-12)}

  {-Model specifieke fout-codes; -DSmodel110: -9819..-9810}

  {cInvld_KeyValue1     = -###;
  cInvld_KeyValue2     = -###;}

  cInvld_ParFromShell_Pr = -9810;
  cInvld_ParFromShell_Ev = -9811;
  cInvld_ParFromShell_Landgebruik = -9812;
  cInvld_ParFromShell_MaandNr = -9813;

  {cInvld_DefaultPar1   = -###;
  cInvld_DefaultPar2   = -###;}

  {cInvld_Init_Val1     = -###;
  cInvld_Init_Val2     = -###;}

var
  Indx: Integer; {-Door de Boot-procedure moet de waarde van deze index worden ingevuld,
                   zodat de snelheidsprocedure 'weet' waar (op de externe parameter-array)
		   hij zijn gegevens moet zoeken}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
                  (zie nDC) }

  {-Globally defined parameters from EP[0]}
  {-###}

  {-Geldige range van key-/parameter/initiele waarden. De waarden van deze  variabelen moeten
    worden ingevuld door de Boot-procedure}
  cMin_ParValue_Pr, cMax_ParValue_Pr,
  cMin_ParValue_Ev, cMax_ParValue_Ev: Double;
  cMin_KeyValue_Landgebruik, cMax_KeyValue_Landgebruik,
  cMin_KeyValue_MaandNr, cMax_KeyValue_MaandNr: Integer;
  {cMin_InitVal1,  cMax_InitVal1,  ###, : Double;}

Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
	if ( nDC > 0 ) then
      ModelProfile.Free;
  end;
end;
				 
				 
Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Deze procedure verschaft de array met afgeleiden 'dydx', gegeven het tijdstip 'x' en
  de toestand die beschreven wordt door de array 'y' en de externe condities die beschreven 
  worden door de 'external parameter-array EP'. Als er geen fout op is getreden bij de 
  berekening van 'dydx' dan wordt in deze procedure de variabele 'IErr' gelijk gemaakt aan de 
  constante 'cNoError'. Opmerking: in de array 'y' staan dus de afhankelijke variabelen,
  terwijl 'x' de onafhankelijke variabele is (meestal de tijd)}
Type
  TGewasFactor = ( f_factor, g_factor );
var
  Landgebruik,
  MaandNr:          Integer;     {-Sleutel-waarden voor de default-tabellen in EP[cEP0]}
  Pr, Ev,                        {-Parameter-waarden afkomstig van de Shell}
  Gewasfactor: Double;
  TypeGewasFactor: TGewasFactor; {-Default parameter-waarden in EP[cEP0]}
  NN: Double;                    {-Afgeleide (berekende) parameter-waarden}
  i: Integer;

Function SetParValuesFromEP0( var IErr: Integer ): Boolean;
  {-Fill globally defined parameters from EP[0]. If memory is allocated here,
    free first with 'try .. except' for those cases that the model is used repeatedly}
begin
  Result := true;
end;

Function SetKeyAndParValues( var IErr: Integer ): Boolean;
  
  Function GetKeyValue_Landgebruik( const x: Double ): Integer;
  begin
    with EP[ indx-1 ].xDep do
      Result := Trunc( Items[ cTb_Landgebruik ].EstimateY( x, Direction ) );
  end;

  Function GetKeyValue_MaandNr( const x: Double ): Integer;
  begin
    with EP[ indx-1 ].xDep do
      Result := Trunc( Items[ cTb_MaandNr ].EstimateY( x, Direction ) );
  end;

  Function GetParFromShell_Pr( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_Pr ].EstimateY( x, Direction );
  end;

  Function GetParFromShell_Ev( const x: Double ): Double;
  begin
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_Ev ].EstimateY( x, Direction );
  end;

  Function GetGewasfactor( const MaandNr, Landgebruik: Integer ): Double;
  begin
    with EP[ cEP0 ].xInDep.Items[ cTb_Gewasfactoren ] do
      Result := GetValue( MaandNr, Landgebruik ); {row, column}
  end;

  Function GetTypeGewasfactor( const Landgebruik: Integer ): TGewasFactor;
  var
    aValue: Double;
  begin
    with EP[ cEP0 ].xInDep.Items[ cTb_ForGfactor ] do begin
      aValue := GetValue( 1, Landgebruik ); {row, column}
      if ( aValue > 0 ) then
        Result := f_factor
      else
        Result := g_factor;
    end;
  end;

begin {-Function SetKeyAndParValues}
  Result := False;

  Landgebruik := GetKeyValue_Landgebruik( x );
  if ( Landgebruik < cMin_KeyValue_Landgebruik ) or ( Landgebruik > cMax_KeyValue_Landgebruik ) then begin
    IErr := cInvld_ParFromShell_Landgebruik; Exit;
  end;

  MaandNr := GetKeyValue_MaandNr( x );
  if ( MaandNr < cMin_KeyValue_MaandNr ) or ( MaandNr > cMax_KeyValue_MaandNr ) then begin
    IErr := cInvld_ParFromShell_MaandNr; Exit;
  end;

  Pr := GetParFromShell_Pr( x );
  if ( Pr < cMin_ParValue_Pr ) or ( Pr > cMax_ParValue_Pr ) then begin
    IErr := cInvld_ParFromShell_Pr; Exit;
  end;

  Ev := GetParFromShell_Ev( x );
  if ( Ev < cMin_ParValue_Ev ) or ( Ev > cMax_ParValue_Ev ) then begin
    IErr := cInvld_ParFromShell_Ev; Exit;
  end;

  Gewasfactor := GetGewasfactor( MaandNr, Landgebruik );

  TypeGewasFactor := GetTypeGewasfactor( Landgebruik );

  Case TypeGewasFactor of
    f_factor: NN := Pr - Gewasfactor * Ev;
    g_factor: NN := Pr * Gewasfactor;
  end;
  
  Result := True; IErr := cNoError;
end; {-Function SetKeyAndParValues}

Function Replace_InitialValues_With_ShellValues( var IErr: Integer): Boolean;
  {-Als de Shell 1-of meer initiele waarden aanlevert voor de array met afhankelijke
    variabelen ('y'), dan kunnen deze waarden hier op deze array worden geplaatst en
    gecontroleerd}
begin
//  IErr := cNoError; Result := True;
//  with EP[ indx-1 ].xDep do
//    y[ ### ] := Items[ cTB_### ].EstimateY( 0, Direction ); {Opm.: x=0}
//  if ( y[ ### ] < cMin_InitVal1 ) or
//     ( y[ ### ] > cMax_InitVal1 ) then begin
//    IErr := cInvld_Init_Val1; Result := False; Exit;
//  end;
  Result := true;
end; {-Replace_InitialValues_With_ShellValues}


begin {-Procedure DerivsProc}
  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;

  if not SetKeyAndParValues( IErr ) then
    exit;

  if ( Context = UpdateYstart ) then begin {-Run fase 1}

    {-Fill globally defined parameters from EP[0]}
    if not SetParValuesFromEP0( IErr ) then Exit;

    {-Optioneel: initiele waarden vervangen door Shell-waarden}
//    if not Replace_InitialValues_With_ShellValues( IErr ) then
//	  Exit;

    {-Bij Shell-gebruik van het model (indx = cBoot2) dan kan het wenselijk zijn de tijd-as
	  van alle Shell-gegevens te converteren, bijvoorbeeld naar jaren}
//     if ( indx = cBoot2 ) then
//        ScaleTimesFromShell( cFromDayToYear, EP ); ###
    IErr := cNoError;

  end else begin {-Run fase 2}

    {-Bereken de array met afgeleiden 'dydx'.
	  Gebruik hierbij 'DCfunc' van 'ModelProfile' i.p.v.
	  'if'-statements! Als hierbij de 'AsSoonAs'-optie
	  wordt gebruikt, moet de statement worden aangevuld
	  met een extra conditie ( Context = Trigger ). Dus
	  bijv.: if DCfunc( AsSoonAs, h, LE, BodemNiveau, Context, cDCfunc0 )
	     and ( Context = Trigger ) then begin...}
    dydx[ cy1 ] := NN;

  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Initialiseer de meest elementaire gegevens van het model. Shell-gegevens worden door deze
    procedure NIET verwerkt}
Procedure SetMinMaxKeyAndParValues;
begin
  with EP[ cEP0 ].xInDep.Items[ cTb_MinMaxValKeys ] do begin
    cMin_ParValue_Pr :=        GetValue( 1, 1 );   {rij, kolom}
    cMax_ParValue_Pr :=        GetValue( 1, 2 );
    cMin_ParValue_Ev :=        GetValue( 1, 3 );
    cMax_ParValue_Ev :=        GetValue( 1, 4 );
    cMin_KeyValue_Landgebruik := Trunc( GetValue( 1, 5 ) );
    cMax_KeyValue_Landgebruik := Trunc( GetValue( 1, 6 ) );
    cMin_KeyValue_MaandNr := Trunc( GetValue( 1, 7 ) );
    cMax_KeyValue_MaandNr := Trunc( GetValue( 1, 8 ) );
    {cMin_InitValue1 :=       GetValue( 1, 5 );
    cMax_InitValue1 :=       GetValue( 1, 6 );}
  end;
end;
Begin
  {$ifdef test}
  ShowMessage( 'Calling DefaultBootEPFromTextFile from function DefaultBootEP with Ord(BootEpArrayOption) = ' + IntToStr(Ord(BootEpArrayOption)) );
  {$endif}
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC, cNrXIndepTblsInEP0,
                                       cNrXdepTblsInEP0, Indx, EP );
  {$ifdef test}
  ShowMessage( 'DefaultBootEPFromTextFile is executed. Result = ' + IntToStr( Result ) );
  {$endif}
  if ( Result = cNoError ) then begin
    SetMinMaxKeyAndParValues;
    SetAnalytic_DerivsProc( True, EP ); {-Ref. 'USpeedProc.pas'}
  end;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze boot-procedure verwerkt alle basisgegevens van het model en leest de Shell-gegevens
    uit een bestand. Na initialisatie met deze boot-procedure is het model dus gereed om
	'te draaien'. Deze procedure kan dus worden gebruikt om het model 'los' van de Shell te 
	testen}
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then 
    exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx, EP );
  if ( Result <> cNoError ) then 
    exit;
  SetReadyToRun( EP);
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze procedure maakt het model gereed voor Shell-gebruik. 
    De xDep-tables in EP[ indx-1 ] worden door deze procedure NIET geinitialiseerd omdat deze
	gegevens door de Shell worden verschaft }
begin
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}
		
begin
  {-Dit zgn. 'DLL-Main-block' wordt uitgevoerd als de DLL voor het eerst in het geheugen wordt
    gezet (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );
end.
