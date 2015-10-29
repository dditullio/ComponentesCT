unit dtdbcoordedit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  StdCtrls, MaskEdit, DB, DBCtrls;

type

  TCoordKind = (ckLatitude, ckLongitude);

  { TDtDBCoordEdit }

  TDtDBCoordEdit = class(TDBEdit)
  private
    FCoordKind: TCoordKind;
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure SetCoordKind(AValue: TCoordKind);
    procedure UpdateControlState(ValueState: boolean);
    procedure UpdateData(Sender: TObject);
    procedure UpdateDisplayText(const NewText: string);
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    property CustomEditMask;
    property EditMask;
    //events
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CoordKind: TCoordKind read FCoordKind write SetCoordKind;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mis Componentes', [TDtDBCoordEdit]);
end;

procedure TDtDBCoordEdit.DataChange(Sender: TObject);
begin
  inherited;
end;

procedure TDtDBCoordEdit.SetCoordKind(AValue: TCoordKind);
begin
  if FCoordKind=AValue then Exit;
  FCoordKind:=AValue;
end;

procedure TDtDBCoordEdit.UpdateControlState(ValueState: boolean);
begin
end;

procedure TDtDBCoordEdit.UpdateData(Sender: TObject);
var
  s: string;
begin
  if (Trim(Text) = '') then
    FDataLink.Field.Value := Null
  else
  begin
    //Completo con ceros si por si lo dejó en blanco
    Text := StringReplace(Text, ' ', '0', [rfReplaceAll]);
    ValidateEdit;
    FDataLink.Field.AsFloat := StrToFloat(Text);
  end;
end;

procedure TDtDBCoordEdit.UpdateDisplayText(const NewText: string);
begin
  inherited;
end;

procedure TDtDBCoordEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
  begin
    FDataLink.Edit;
    if Key = VK_DELETE then
    begin
      FDatalink.Field.Value := Null;
      Key := VK_UNKNOWN;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TDtDBCoordEdit.KeyPress(var Key: char);
var
  ss: integer;
begin
  //  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and not
    FDataLink.Field.IsValidChar(Key) then
  begin
    Beep;
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    //#27:
    //begin
    //  FDataLink.Reset;
    //  SelectAll;
    //  Key := #0;
    //end;
  end;

  ss := selstart;
  if ss <= 7 then
    sellength := 1;
  if ss = 0 then
  begin
    if not (key in ['0'..'9']) then
      key := #0;
  end
  else if ss = 1 then
  begin
    if not ((EditText[1] in ['0'..'8']) and (key in ['0'..'9'])) and
      not ((EditText[1] = '9') and (key in ['0'])) then
      key := #0;
  end
  else if ss = 2 then
  begin
    if not (key in ['0'..'5']) then
      key := #0;
  end
  else if ss = 3 then
  begin
    if not (key in ['0'..'9']) then
      key := #0;
  end
  else if ss = 4 then
  begin
    if not (key in [',','.']) then
      key := '.';
  end
  else if ss = 5 then
  begin
    if not (key in ['0'..'9']) then
      key := #0;
  end
  else if ss = 6 then
  begin
    if not (key in ['0'..'9']) then
      key := #0;
  end
  else if ss >= 8 then
  begin
    key := #0;
  end;

  if key <> #0 then
    inherited
  else
    Beep;

end;

constructor TDtDBCoordEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
  FDataLink.OnUpdateData := UpdateData;
  CustomEditMask := True;
  FCoordKind:=ckLatitude;
  EditMask := '####.##;1;_';
end;

end.
