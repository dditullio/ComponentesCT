unit DtDBTimeEdit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  StdCtrls, MaskEdit, DB, DBCtrls;

type

  { TDtDBTimeEdit }

  TDtDBTimeEdit = class(TDBEdit)
  private
    FDataLink: TFieldDataLink;
    procedure UpdateData(Sender: TObject);
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    property CustomEditMask;
    property EditMask;
    //events
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mis Componentes', [TDtDBTimeEdit]);
end;

procedure TDtDBTimeEdit.UpdateData(Sender: TObject);
var
  s: string;
begin
  if (Text = '  :  ') or (Trim(Text) = '') then
    FDataLink.Field.Value := Null
  else
  begin
    //Si pone 1 solo dígito de la hora, lo coloco en su lugar
    s:=Text;
    if (Length(Text)>=2) and (Text[1]<>'') and (Text[2]=#32) then
    begin
      s:=Text;
      s[2]:=s[1];
      s[1]:='0';
      Text:=s;
    end;
    //Completo con ceros si por si lo dejó en blanco
    Text := StringReplace(Text, ' ', '0', [rfReplaceAll]);

    ValidateEdit;

    if Int(FDataLink.Field.AsDateTime) = 0 then
      FDataLink.Field.AsDateTime := 2 {NullDate} + Frac(StrToTime(Text))
    else
      FDataLink.Field.AsDateTime :=
        Int(FDataLink.Field.AsDateTime) + Frac(StrToTime(Text));

  end;
end;

procedure TDtDBTimeEdit.KeyDown(var Key: word; Shift: TShiftState);
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

procedure TDtDBTimeEdit.KeyPress(var Key: char);
{begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    Beep;
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;
}
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
    if not (key in ['0'..'2']) then
      key := #0;
  end
  else if ss = 1 then
  begin
    if not ((EditText[1] in ['0'..'1']) and (key in ['0'..'9'])) and
      not ((EditText[1] = '2') and (key in ['0'..'3'])) then
      key := #0;
  end
  else if ss = 2 then
  begin
    if not (key in [':']) then
      key := ':';
  end
  else if ss = 3 then
  begin
    if not (key in ['0'..'5']) then
      key := #0;
  end
  else if ss = 4 then
  begin
    if not (key in ['0'..'9']) then
      key := #0;
  end
  else if ss = 5 then
  begin
    if not (key in [':']) then
      key := ':';
  end
  else if ss = 6 then
  begin
    if not (key in ['0'..'5']) then
      key := #0;
  end
  else if ss = 7 then
  begin
    if not (key in ['0'..'9']) then
      key := #0;
  end
  else if ss >= 8 then
  begin
    key := #0;
  end;

  if key <> #0 then
    inherited KeyPress(key)
  else
    Beep;

end;

constructor TDtDBTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
  FDataLink.OnUpdateData := UpdateData;
  CustomEditMask := True;
  EditMask := '!##:##;1;_';
end;

end.
