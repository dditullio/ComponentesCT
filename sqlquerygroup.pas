unit SQLQueryGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, db;

type
  { TSQLQueryComponent }

  TSQLQueryComponent = class(TCollectionItem)
  private
    FAutoincGenerator: boolean;
    FIncludeInClose: boolean;
    FIncludeInOpen: boolean;
    FIncludeInUpdate: Boolean;
    FRepeatCount: Cardinal;
    FSQLQuery: TSQLQuery;
    function GetNewId: LongInt;
    procedure SetAutoincGenerator(AValue: boolean);
    procedure SetIncludeInCLose(AValue: boolean);
    procedure SetIncludeInOpen(AValue: boolean);
    procedure SetIncludeInUpdate(AValue: Boolean);
    procedure SetSQLQuery(AValue: TSQLQuery);
    procedure SQLQueryComponentChanged(ASender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    property NewId:LongInt read GetNewId;
  published
    property SQLQuery:TSQLQuery read FSQLQuery write SetSQLQuery;
    property IncludeInOpen:boolean read FIncludeInOpen write SetIncludeInOpen default true;
    property IncludeInClose:boolean read FIncludeInClose write SetIncludeInClose default true;
    property IncludeInUpdate:Boolean read FIncludeInUpdate write SetIncludeInUpdate default false;
    property AutoincGenerator: boolean read FAutoincGenerator write SetAutoincGenerator default false;
  end;

  TSQLQueryGroup = class;

  { TSQLQueryEnumerator }

  TSQLQueryEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TSQLQueryComponent;
    property Current: TSQLQueryComponent read GetCurrent;
  end;

  { TSQLQueryList }

  TSQLQueryList = class(TCollection)
  private
    FOwner: TSQLQueryGroup;
    function GetSQLQueryComponent(AIndex: Integer): TSQLQueryComponent;
  protected
    procedure Changed;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TSQLQueryGroup);
    function GetEnumerator: TSQLQueryEnumerator; inline;
    property SQLQuery[AIndex: Integer]: TSQLQueryComponent read GetSQLQueryComponent; default;
  end;

  { TSQLQueryGroup }

  TSQLQueryGroup = class(TComponent)
  private
    FLastErrorCode: integer;
    FLastErrorMsg: string;
    FNombreIdMaestro: string;
    FQueryMaestro: TSQLQuery;
    FSQLQueryList: TSQLQueryList;
    function GetDatabase: TDatabase;
    procedure SetNombreIdMaestro(AValue: string);
    procedure SetQueryMaestro(AValue: TSQLQuery);
    procedure SetSQLQueryList(AValue: TSQLQueryList);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Database: TDatabase read GetDatabase;
    property LastErrorCode: integer read FLastErrorCode;
    property LastErrorMsg: string read FLastErrorMsg;
    function ApplyUpdates(MaxErrors:Integer):boolean;
    procedure CancelUpdates;
    function UpdatesPending: boolean;
    procedure Open;
    procedure Refresh;
    procedure Close;
    procedure Cancel;
    procedure Post;
    procedure CheckBrowseMode;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetLastAutoInc:LargeInt;
  published
    property QueryMaestro:TSQLQuery read FQueryMaestro write SetQueryMaestro;
    property NombreIdMaestro: string read FNombreIdMaestro write SetNombreIdMaestro;
    property QuerysComplementarios: TSQLQueryList read FSQLQueryList write SetSQLQueryList;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Mis Componentes', [TSQLQueryGroup]);
end;

{ TSQLQueryComponent }

procedure TSQLQueryComponent.SQLQueryComponentChanged(ASender: TObject);
begin
  TSQLQueryList(Collection).Changed;
end;

function TSQLQueryComponent.GetNewId: LongInt;
begin
  Result:=-1;
end;

procedure TSQLQueryComponent.SetAutoincGenerator(AValue: boolean);
begin
  if FAutoincGenerator=AValue then Exit;
  FAutoincGenerator:=AValue;
end;

procedure TSQLQueryComponent.SetIncludeInCLose(AValue: boolean);
begin
  if FIncludeInClose=AValue then Exit;
  FIncludeInClose:=AValue;
end;

procedure TSQLQueryComponent.SetIncludeInOpen(AValue: boolean);
begin
  if FIncludeInOpen=AValue then Exit;
  FIncludeInOpen:=AValue;
end;

procedure TSQLQueryComponent.SetIncludeInUpdate(AValue: Boolean);
begin
  if FIncludeInUpdate=AValue then Exit;
  FIncludeInUpdate:=AValue;
end;

procedure TSQLQueryComponent.SetSQLQuery(AValue: TSQLQuery);
begin
  if FSQLQuery=AValue then Exit;
  FSQLQuery:=AValue;
  SQLQueryComponentChanged(Self);
end;

function TSQLQueryComponent.GetDisplayName: string;
begin
  //Result:=inherited GetDisplayName;
  if Assigned(SQLQuery) then
    Result:=SQLQuery.Name
  else
    Result:=inherited GetDisplayName;
end;

constructor TSQLQueryComponent.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FIncludeInOpen:=True;
  FIncludeInClose:=True;
end;

destructor TSQLQueryComponent.Destroy;
begin
  inherited Destroy;
end;

procedure TSQLQueryComponent.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ TSQLQueryGroup }

procedure TSQLQueryGroup.SetSQLQueryList(AValue: TSQLQueryList);
begin
  if FSQLQueryList = AValue then exit;
  FSQLQueryList := AValue;
end;

procedure TSQLQueryGroup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FQueryMaestro <> nil) and (AComponent = QueryMaestro) then
      QueryMaestro := nil;
  end;
end;

function TSQLQueryGroup.GetDatabase: TDatabase;
var
  i:Integer;
  db: TDatabase;
begin
  if Assigned(QueryMaestro) and Assigned(QueryMaestro.DataBase) then
  begin
    db:=QueryMaestro.DataBase;
  end
  else
  begin
    db:=nil;
    i:=0;
    while (i<= QuerysComplementarios.Count-1) and (not Assigned(db)) do
    begin
      if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (Assigned(QuerysComplementarios.SQLQuery[i].SQLQuery.DataBase)) then
      begin
        db:=QuerysComplementarios.SQLQuery[i].SQLQuery.DataBase;
      end;
      inc(i);
    end;
  end;
  Result:= db;
end;

procedure TSQLQueryGroup.SetNombreIdMaestro(AValue: string);
begin
  if FNombreIdMaestro=AValue then Exit;
  FNombreIdMaestro:=AValue;
end;

procedure TSQLQueryGroup.SetQueryMaestro(AValue: TSQLQuery);
var
  sIdMaestro: string;
begin
  if FQueryMaestro=AValue then Exit;
  FQueryMaestro:=AValue;
  //Si el SQL está vacío, cargo la plantilla con el formato de búsqueda
  if (FQueryMaestro <> nil) and (FQueryMaestro.SQL.Count=0) then
  begin
    if FNombreIdMaestro<>'' then
      sIdMaestro:=FNombreIdMaestro
    else
      sIdMaestro:='Id';
    FQueryMaestro.SQL.Add('SELECT <campo_id>, <campo_1>, ...');
    FQueryMaestro.SQL.Add('FROM <tabla>');
    FQueryMaestro.SQL.Add('WHERE <campo_id>=:'+sIdMaestro);
  end;
end;

constructor TSQLQueryGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNombreIdMaestro:='Id';
  FSQLQueryList := TSQLQueryList.Create(Self);
  FLastErrorCode:=0;
  FLastErrorMsg:='';
end;

destructor TSQLQueryGroup.Destroy;
begin
  FreeAndNil(FSQLQueryList);
  inherited Destroy;
end;

function TSQLQueryGroup.ApplyUpdates(MaxErrors: Integer):boolean;
var
  i:Integer;
  r:boolean;
begin
  r:=false;
  StartTransaction;
  try
    if Assigned(QueryMaestro) and QueryMaestro.Active then
      QueryMaestro.ApplyUpdates(MaxErrors);
    for i:=0 to QuerysComplementarios.Count-1 do
    begin
      if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
        and (QuerysComplementarios.SQLQuery[i].IncludeInUpdate) then
      begin
        QuerysComplementarios.SQLQuery[i].SQLQuery.ApplyUpdates(MaxErrors);
      end;
    end;
    Commit;
    r:=true;
  except
    Rollback;
  end;
  Result:=r;
end;

procedure TSQLQueryGroup.CancelUpdates;
var
  i:Integer;
begin
  if Assigned(QueryMaestro) and QueryMaestro.Active then
    QueryMaestro.CancelUpdates;
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInUpdate) then
    begin
      QuerysComplementarios.SQLQuery[i].SQLQuery.CancelUpdates;
    end;
  end;
end;

function TSQLQueryGroup.UpdatesPending: boolean;
var
  i:Integer;
  UpdPend: boolean;
begin
  UpdPend:=false;
  if Assigned(QueryMaestro) and QueryMaestro.Active then
    if QueryMaestro.ChangeCount>0 then
      UpdPend:=true;
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInUpdate) then
    begin
      if QuerysComplementarios.SQLQuery[i].SQLQuery.ChangeCount>0 then
        UpdPend:=true;
    end;
  end;
  Result:=UpdPend;
end;

procedure TSQLQueryGroup.Open;
var
  i:Integer;
begin
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (not QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInOpen) then
    begin
      QuerysComplementarios.SQLQuery[i].SQLQuery.Open;
    end;
  end;
end;

procedure TSQLQueryGroup.Refresh;
var
  i:Integer;
begin
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (not QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInOpen) then
    begin
      QuerysComplementarios.SQLQuery[i].SQLQuery.Close;
      QuerysComplementarios.SQLQuery[i].SQLQuery.Open;
    end;
  end;
end;

procedure TSQLQueryGroup.Close;
var
  i:Integer;
begin
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInClose) then
    begin
      QuerysComplementarios.SQLQuery[i].SQLQuery.Close;
    end;
  end;
end;

procedure TSQLQueryGroup.Cancel;
var
  i:Integer;
begin
  if Assigned(QueryMaestro) and QueryMaestro.Active then
    if QueryMaestro.State in [dsEdit, dsInsert] then
      QueryMaestro.Cancel;
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInUpdate) then
    begin
      if QuerysComplementarios.SQLQuery[i].SQLQuery.State in [dsEdit, dsInsert] then
        QuerysComplementarios.SQLQuery[i].SQLQuery.Cancel;
    end;
  end;
end;

procedure TSQLQueryGroup.Post;
var
  i:Integer;
begin
  if Assigned(QueryMaestro) and QueryMaestro.Active then
    if QueryMaestro.State in [dsEdit, dsInsert] then
      QueryMaestro.Post;
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInUpdate) then
    begin
      if QuerysComplementarios.SQLQuery[i].SQLQuery.State in [dsEdit, dsInsert] then
        QuerysComplementarios.SQLQuery[i].SQLQuery.Post;
    end;
  end;
end;

procedure TSQLQueryGroup.CheckBrowseMode;
var
  i:Integer;
begin
  if Assigned(QueryMaestro) and QueryMaestro.Active then
    QueryMaestro.CheckBrowseMode;
  for i:=0 to QuerysComplementarios.Count-1 do
  begin
    if Assigned (QuerysComplementarios.SQLQuery[i]) and Assigned (QuerysComplementarios.SQLQuery[i].SQLQuery) and (QuerysComplementarios.SQLQuery[i].SQLQuery.Active)
      and (QuerysComplementarios.SQLQuery[i].IncludeInUpdate) then
    begin
      QuerysComplementarios.SQLQuery[i].SQLQuery.CheckBrowseMode;
    end;
  end;
end;

procedure TSQLQueryGroup.StartTransaction;
begin
  if Assigned(Database) and (Database is TSQLConnection) then
    (Database as TSQLConnection).ExecuteDirect('START TRANSACTION;');
end;

procedure TSQLQueryGroup.Commit;
begin
  if Assigned(Database) and (Database is TSQLConnection) then
    (Database as TSQLConnection).ExecuteDirect('COMMIT;');
end;

procedure TSQLQueryGroup.Rollback;
begin
  if Assigned(Database) and (Database is TSQLConnection) then
    (Database as TSQLConnection).ExecuteDirect('ROLLBACK;');
end;

function TSQLQueryGroup.GetLastAutoInc: LargeInt;
begin
  //No implementado
  Result:=-1;
end;

{ TSQLQueryList }

function TSQLQueryList.GetSQLQueryComponent(AIndex: Integer
  ): TSQLQueryComponent;
begin
  Result := Items[AIndex] as TSQLQueryComponent;
end;

procedure TSQLQueryList.Changed;
begin
end;

function TSQLQueryList.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

constructor TSQLQueryList.Create(AOwner: TSQLQueryGroup);
begin
  inherited Create(TSQLQueryComponent);
  FOwner := AOwner;
end;

function TSQLQueryList.GetEnumerator: TSQLQueryEnumerator;
begin
  Result := TSQLQueryEnumerator.Create(Self);
end;

{ TSQLQueryEnumerator }

function TSQLQueryEnumerator.GetCurrent: TSQLQueryComponent;
begin
  Result := TSQLQueryComponent(inherited GetCurrent);
end;

end.
