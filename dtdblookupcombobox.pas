unit DtDBLookupComboBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DbCtrls, strutils, LMessages;

type

  //todo: implement RecoverMode
  //TRecoverMode = (rmNone, rmClear, rmRestore);

  TValidateDataEvent = procedure(Sender: TObject; var NewData: String; var IsValid: Boolean) of object;

  TValueChangeEvent = procedure(Sender: TObject; StateChanged: Boolean) of object;

  TDBValidateEditOption = (voNullValueAsError);

  TDBValidateEditOptions = set of TDBValidateEditOption;

  { TCustomDtDBLookupComboBox }

  TCustomDtDBLookupComboBox = class(TDBLookupComboBox)
  private
    FDataLink: TFieldDataLink;
    FInvalidValueColor: TColor;
    FInvalidValueMessage: String;
    FOldDataChange: TNotifyEvent;
    FOnValueChange: TValueChangeEvent;
    FOptions: TDBValidateEditOptions;
    //FRecoverMode: TRecoverMode;
    FValueIsValid: Boolean;
    procedure DataChange(Sender: TObject);
    procedure UpdateControlState(ValueState: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure UpdateDisplayText(const NewText: String);
  protected
    function DoValidateData(var NewData: String): Boolean; virtual; abstract;
    //events
    property OnValueChange: TValueChangeEvent read FOnValueChange write FOnValueChange;
  public
    constructor Create(AOwner: TComponent); override;
    property ValueIsValid: Boolean read FValueIsValid;
  published
    property InvalidValueColor: TColor read FInvalidValueColor write FInvalidValueColor default clWindow;
    property InvalidValueMessage: String read FInvalidValueMessage write FInvalidValueMessage;
    property Options: TDBValidateEditOptions read FOptions write FOptions default [];
    //property RecoverMode: TRecoverMode read FRecoverMode write FRecoverMode default rmNone;
  end;

  { TDtDBLookupComboBox }

  TDtDBLookupComboBox = class(TCustomDtDBLookupComboBox)
  private
    FOnValidateData: TValidateDataEvent;
  protected
    function DoValidateData(var NewData: String): Boolean; override;
  published
    property OnValidateData: TValidateDataEvent read FOnValidateData write FOnValidateData;
    property OnValueChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I dtdblookupcombobox_icon.lrs}
  RegisterComponents('Mis Componentes',[TDtDBLookupComboBox]);
end;

procedure TCustomDtDBLookupComboBox.DataChange(Sender: TObject);
begin
  UpdateControlState((FDataLink.Field = nil) or (FDataLink.Field.Value <> Null) or
    not (voNullValueAsError in FOptions));
  FOldDataChange(Sender);
end;

procedure TCustomDtDBLookupComboBox.UpdateControlState(ValueState: Boolean);
begin
  FValueIsValid := ValueState;
  if FValueIsValid then
    Color := clWindow
  else
    Color := FInvalidValueColor;
end;

procedure TCustomDtDBLookupComboBox.UpdateData(Sender: TObject);
var
  S: String;
  OldValueIsValid: Boolean;
begin
  S := Text;
  OldValueIsValid := FValueIsValid;
  FValueIsValid := DoValidateData(S);
  if Assigned(FOnValueChange) then
    FOnValueChange(Self, FValueIsValid <> OldValueIsValid);
  if FValueIsValid then
  begin
    FDataLink.Field.AsString := S;
    Color := clWindow;
  end
  else
  begin
    {$ifdef DEBUG_VALIDATEEDIT}
    Logger.Send('Before ShowMessage');
    {$endif}
    if FInvalidValueMessage <> '' then
      ShowMessage(AnsiReplaceText(FInvalidValueMessage, '$(NewValue)', S));
    {$ifdef DEBUG_VALIDATEEDIT}
    Logger.Send('After ShowMessage');
    {$endif}
    Color := FInvalidValueColor;
    //todo: implement RecoverMode
    {
    case FRecoverMode of
      rmNone:
        begin
          Color := FInvalidValueColor;
          //UpdateDisplayText();
        end;
      rmClear:
        begin
          UpdateDisplayText('');
          UpdateControlState(not (voNullValueAsError in FOptions));
        end;
      rmRestore: FDataLink.Reset;
    end;
    }
  end;
end;

procedure TCustomDtDBLookupComboBox.UpdateDisplayText(const NewText: String);
begin
  //avoid calling the UpdateData event
  FDataLink.OnUpdateData := nil;
  try
    Text := NewText;
  finally
    FDataLink.OnUpdateData := @UpdateData;
  end;
end;

constructor TCustomDtDBLookupComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
  FOldDataChange := FDataLink.OnDataChange;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
  //FRecoverMode := rmNone;
  FInvalidValueColor := clRed;
end;

{ TDtDBLookupComboBox }

function TDtDBLookupComboBox.DoValidateData(var NewData: String): Boolean;
begin
  Result := True;
  if Assigned(FOnValidateData) then
    FOnValidateData(Self, NewData, Result);
end;

end.
