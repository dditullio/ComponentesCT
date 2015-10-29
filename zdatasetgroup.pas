unit zdatasetgroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, ZDataset, ZAbstractConnection, ZAbstractDataset,
  ZAbstractRODataset, ZAbstractTable, DB, ZDbcIntfs, Dialogs;

type
  { TDatasetComponent }

  TZDatasetComponent = class(TCollectionItem)
  private
    FAutoincGenerator: boolean;
    FIncludeInClose: boolean;
    FIncludeInOpen: boolean;
    FIncludeInUpdate: boolean;
    FRepeatCount: cardinal;
    FDataset: TZAbstractDataset;
    function GetNewId: longint;
    procedure SetAutoincGenerator(AValue: boolean);
    procedure SetIncludeInCLose(AValue: boolean);
    procedure SetIncludeInOpen(AValue: boolean);
    procedure SetIncludeInUpdate(AValue: boolean);
    procedure SetDataset(AValue: TZAbstractDataset);
    procedure DatasetComponentChanged(ASender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    property NewId: longint read GetNewId;
  published
    property Dataset: TZAbstractDataset read FDataset write SetDataset;
    property IncludeInOpen: boolean
      read FIncludeInOpen write SetIncludeInOpen default True;
    property IncludeInClose: boolean
      read FIncludeInClose write SetIncludeInClose default True;
    property IncludeInUpdate: boolean read FIncludeInUpdate
      write SetIncludeInUpdate default False;
    property AutoincGenerator: boolean read FAutoincGenerator
      write SetAutoincGenerator default False;
  end;

  TZDatasetGroup = class;

  { TZDatasetEnumerator }

  TZDatasetEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TZDatasetComponent;
    property Current: TZDatasetComponent read GetCurrent;
  end;

  { TZDatasetList }

  TZDatasetList = class(TCollection)
  private
    FOwner: TZDatasetGroup;
    function GetDatasetComponent(AIndex: integer): TZDatasetComponent;
  protected
    procedure Changed;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TZDatasetGroup);
    function GetEnumerator: TZDatasetEnumerator; inline;
    property Dataset[AIndex: integer]: TZDatasetComponent read GetDatasetComponent;
      default;
  end;

  { TZDatasetGroup }

  TZDatasetGroup = class(TComponent)
  private
    FLastErrorCode: integer;
    FLastErrorMsg: string;
    FCampoIdMaestro: string;
    FDatasetMaestro: TZAbstractDataset;
    FDatasetList: TZDatasetList;
    function GetDatabase: TZAbstractConnection;
    procedure SetCampoIdMaestro(AValue: string);
    procedure SetDatasetMaestro(AValue: TZAbstractDataset);
    procedure SetDatasetList(AValue: TZDatasetList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Connection: TZAbstractConnection read GetDatabase;
    property LastErrorCode: integer read FLastErrorCode;
    property LastErrorMsg: string read FLastErrorMsg;
    function ApplyUpdates: boolean;
    procedure CancelUpdates;
    function UpdatesPending: boolean;
    procedure Open; // Abre todos los querys secundarios
    procedure OpenReadOnly; //Sólo abre los querys secundarios que no se actualizan
    procedure OpenUpdatable; //Sólo abre los querys secundarios que sí se actualizan
    procedure Refresh;
    procedure Close;
    procedure Cancel;
    procedure Post;
    procedure CheckBrowseMode;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetLastAutoInc: LargeInt;
  published
    property DatasetMaestro: TZAbstractDataset
      read FDatasetMaestro write SetDatasetMaestro;
    property CampoIdMaestro: string read FCampoIdMaestro write SetCampoIdMaestro;
    property DatasetsComplementarios: TZDatasetList
      read FDatasetList write SetDatasetList;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Mis Componentes', [TZDatasetGroup]);
end;

{ TZDatasetComponent }

procedure TZDatasetComponent.DatasetComponentChanged(ASender: TObject);
begin
  TZDatasetList(Collection).Changed;
end;

function TZDatasetComponent.GetNewId: longint;
begin

end;

procedure TZDatasetComponent.SetAutoincGenerator(AValue: boolean);
begin
  if FAutoincGenerator = AValue then
    Exit;
  FAutoincGenerator := AValue;
end;

procedure TZDatasetComponent.SetIncludeInCLose(AValue: boolean);
begin
  if FIncludeInClose = AValue then
    Exit;
  FIncludeInClose := AValue;
end;

procedure TZDatasetComponent.SetIncludeInOpen(AValue: boolean);
begin
  if FIncludeInOpen = AValue then
    Exit;
  FIncludeInOpen := AValue;
end;

procedure TZDatasetComponent.SetIncludeInUpdate(AValue: boolean);
begin
  if FIncludeInUpdate = AValue then
    Exit;
  FIncludeInUpdate := AValue;
end;

procedure TZDatasetComponent.SetDataset(AValue: TZAbstractDataset);
begin
  if FDataset = AValue then
    Exit;
  FDataset := AValue;
  DatasetComponentChanged(Self);
end;

function TZDatasetComponent.GetDisplayName: string;
begin
  //Result:=inherited GetDisplayName;
  if Assigned(Dataset) then
    Result := Dataset.Name
  else
    Result := inherited GetDisplayName;
end;

constructor TZDatasetComponent.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIncludeInOpen := True;
  FIncludeInClose := True;
end;

destructor TZDatasetComponent.Destroy;
begin
  inherited Destroy;
end;

procedure TZDatasetComponent.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ TZDatasetGroup }

procedure TZDatasetGroup.SetDatasetList(AValue: TZDatasetList);
begin
  if FDatasetList = AValue then
    exit;
  FDatasetList := AValue;
end;

procedure TZDatasetGroup.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FDatasetMaestro <> nil) and (AComponent = DatasetMaestro) then
      DatasetMaestro := nil;
  end;
end;

function TZDatasetGroup.GetDatabase: TZAbstractConnection;
var
  i: integer;
  db: TZAbstractConnection;
begin
  if Assigned(DatasetMaestro) and Assigned(DatasetMaestro.Connection) then
  begin
    db := DatasetMaestro.Connection;
  end
  else
  begin
    db := nil;
    i := 0;
    while (i <= DatasetsComplementarios.Count - 1) and (not Assigned(db)) do
    begin
      if Assigned(DatasetsComplementarios.Dataset[i]) and
        Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
        (Assigned(DatasetsComplementarios.Dataset[i].Dataset.Connection)) then
      begin
        db := DatasetsComplementarios.Dataset[i].Dataset.Connection;
      end;
      Inc(i);
    end;
  end;
  Result := db;
end;

procedure TZDatasetGroup.SetCampoIdMaestro(AValue: string);
begin
  if FCampoIdMaestro = AValue then
    Exit;
  FCampoIdMaestro := AValue;
end;

procedure TZDatasetGroup.SetDatasetMaestro(AValue: TZAbstractDataset);
var
  sIdMaestro: string;
begin
  if FDatasetMaestro = AValue then
    Exit;
  FDatasetMaestro := AValue;
  //Si el SQL está vacío, cargo la plantilla con el formato de búsqueda
  if (FDatasetMaestro <> nil) and (FDatasetMaestro is TZQuery) and
    ((FDatasetMaestro as TZQuery).SQL.Count = 0) then
  begin
    if FCampoIdMaestro <> '' then
      sIdMaestro := FCampoIdMaestro
    else
      sIdMaestro := 'Id';
    (FDatasetMaestro as TZQuery).SQL.Add('SELECT <campo_id>, <campo_1>, ...');
    (FDatasetMaestro as TZQuery).SQL.Add('FROM <tabla>');
    (FDatasetMaestro as TZQuery).SQL.Add('WHERE <campo_id>=:' + sIdMaestro);
  end;
end;

constructor TZDatasetGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCampoIdMaestro := 'Id';
  FDatasetList := TZDatasetList.Create(Self);
  FLastErrorCode := 0;
  FLastErrorMsg := '';
end;

destructor TZDatasetGroup.Destroy;
begin
  FreeAndNil(FDatasetList);
  inherited Destroy;
end;

function TZDatasetGroup.ApplyUpdates: boolean;
var
  i: integer;
  r: boolean;
begin
  r := False;
  StartTransaction;
  try
    if Assigned(DatasetMaestro) and DatasetMaestro.Active then
    begin
      DatasetMaestro.ApplyUpdates;
      DatasetMaestro.CommitUpdates;
    end;
    for i := 0 to DatasetsComplementarios.Count - 1 do
    begin
      if Assigned(DatasetsComplementarios.Dataset[i]) and
        Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
        (DatasetsComplementarios.Dataset[i].Dataset.Active) and
        (DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
      begin
        DatasetsComplementarios.Dataset[i].Dataset.ApplyUpdates;
        DatasetsComplementarios.Dataset[i].Dataset.CommitUpdates;
      end;
    end;
    Commit;
    r := True;
    //except on E: EZSQLThrowable do
    //  raise EZDatabaseError.CreateFromException(E);
  except
    on E: EZSQLThrowable do
    begin
      Rollback;
      MessageDlg('Ocurrió un error al guarar los datos: '+CHR(13)+CHR(13)+'Código de error: '+IntToStr((E as EZSQLThrowable).ErrorCode)+CHR(13)+CHR(13)+'Detalle: '+CHR(13)+StringReplace((E as EZSQLThrowable).Message,'SQL Error: ','',[rfReplaceAll]), mtError, [mbClose],0);
    end;
  end;
  Result := r;
end;

procedure TZDatasetGroup.CancelUpdates;
var
  i: integer;
begin
  if Assigned(DatasetMaestro) and DatasetMaestro.Active then
    DatasetMaestro.CancelUpdates;
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
    begin
      DatasetsComplementarios.Dataset[i].Dataset.CancelUpdates;
    end;
  end;
end;

function TZDatasetGroup.UpdatesPending: boolean;
var
  i: integer;
  UpdPend: boolean;
begin
  UpdPend := False;
  if Assigned(DatasetMaestro) and DatasetMaestro.Active then
  begin
//    if DatasetMaestro.Modified then
    if (DatasetMaestro.UpdatesPending) then
      UpdPend := True;
  end;
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
    begin
//      if DatasetsComplementarios.Dataset[i].Dataset.Modified then
      if (DatasetsComplementarios.Dataset[i].Dataset.UpdatesPending) or (DatasetsComplementarios.Dataset[i].Dataset.Modified) then
        UpdPend := True;
    end;
  end;
  Result := UpdPend;
end;

procedure TZDatasetGroup.Open;
var
  i: integer;
begin
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (not DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInOpen) then
    begin
      DatasetsComplementarios.Dataset[i].Dataset.Open;
    end;
  end;
end;

procedure TZDatasetGroup.OpenReadOnly;
var
  i: integer;
begin
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (not DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInOpen)
      and (not DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
    begin
      DatasetsComplementarios.Dataset[i].Dataset.Open;
    end;
  end;
end;

procedure TZDatasetGroup.OpenUpdatable;
var
  i: integer;
begin
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (not DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInOpen)
      and (DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
    begin
      DatasetsComplementarios.Dataset[i].Dataset.Open;
    end;
  end;
end;

procedure TZDatasetGroup.Refresh;
var
  i: integer;
begin
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (not DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInOpen) then
    begin
      DatasetsComplementarios.Dataset[i].Dataset.Close;
      DatasetsComplementarios.Dataset[i].Dataset.Open;
    end;
  end;
end;

procedure TZDatasetGroup.Close;
var
  i: integer;
begin
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInClose) then
    begin
      DatasetsComplementarios.Dataset[i].Dataset.Close;
    end;
  end;
end;

procedure TZDatasetGroup.Cancel;
var
  i: integer;
begin
  if Assigned(DatasetMaestro) and DatasetMaestro.Active then
    if DatasetMaestro.State in [dsEdit, dsInsert] then
      DatasetMaestro.Cancel;
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
    begin
      if DatasetsComplementarios.Dataset[i].Dataset.State in [dsEdit, dsInsert] then
        DatasetsComplementarios.Dataset[i].Dataset.Cancel;
    end;
  end;
end;

procedure TZDatasetGroup.Post;
var
  i: integer;
begin
  if Assigned(DatasetMaestro) and DatasetMaestro.Active then
    if DatasetMaestro.State in [dsEdit, dsInsert] then
      DatasetMaestro.Post;
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
    begin
      if DatasetsComplementarios.Dataset[i].Dataset.State in [dsEdit, dsInsert] then
        DatasetsComplementarios.Dataset[i].Dataset.Post;
    end;
  end;
end;

procedure TZDatasetGroup.CheckBrowseMode;
var
  i: integer;
begin
  if Assigned(DatasetMaestro) and DatasetMaestro.Active then
    DatasetMaestro.CheckBrowseMode;
  for i := 0 to DatasetsComplementarios.Count - 1 do
  begin
    if Assigned(DatasetsComplementarios.Dataset[i]) and
      Assigned(DatasetsComplementarios.Dataset[i].Dataset) and
      (DatasetsComplementarios.Dataset[i].Dataset.Active) and
      (DatasetsComplementarios.Dataset[i].IncludeInUpdate) then
    begin
      DatasetsComplementarios.Dataset[i].Dataset.CheckBrowseMode;
    end;
  end;
end;

procedure TZDatasetGroup.StartTransaction;
begin
  if Assigned(Connection) then
    Connection.StartTransaction;
end;

procedure TZDatasetGroup.Commit;
begin
  if Assigned(Connection) then
    Connection.Commit;
end;

procedure TZDatasetGroup.Rollback;
begin
  if Assigned(Connection) then
    Connection.Rollback;
end;

function TZDatasetGroup.GetLastAutoInc: LargeInt;
begin
  //No implementado
  Result := -1;
end;

{ TZDatasetList }

function TZDatasetList.GetDatasetComponent(AIndex: integer): TZDatasetComponent;
begin
  Result := Items[AIndex] as TZDatasetComponent;
end;

procedure TZDatasetList.Changed;
begin
end;

function TZDatasetList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

constructor TZDatasetList.Create(AOwner: TZDatasetGroup);
begin
  inherited Create(TZDatasetComponent);
  FOwner := AOwner;
end;

function TZDatasetList.GetEnumerator: TZDatasetEnumerator;
begin
  Result := TZDatasetEnumerator.Create(Self);
end;

{ TZDatasetEnumerator }

function TZDatasetEnumerator.GetCurrent: TZDatasetComponent;
begin
  Result := TZDatasetComponent(inherited GetCurrent);
end;

end.
