unit ControladorEdicion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,LCLClasses,
  Buttons, sqldb, SQLQueryGroup, StdCtrls, DbCtrls, ExtCtrls,LCLType, DateTimePicker;
const
  ED_AGREGAR = 0;
  ED_MODIFICAR = 1;
  ED_ELIMINAR = 2;
  ED_INDETERMINADO = 3;
  BC_ACEPTAR = '&Aceptar';
  BC_CANCELAR = '&Cancelar';
  BC_CERRAR = 'Ce&rrar';
  BC_GUARDAR = '&Guardar';
  BC_ELIMINAR = '&Eliminar';
  BC_SALIR = '&Salir';

type

  //Evento que se dispara antes de aceptar o guardar. Permite realizar
  //validaciones en el formulario  e indicar si todo está OK para continuar
  TValidateEvent = procedure(Sender : TObject; var ValidacionOK : boolean) of object;

  { TDummyControl }
  //Se usa estr control de la clase TWinControl para poder implementar
  //el método KeyDown que se asigna a los controles del formulario

  TDummyControl = class(TWinControl)
    public
      procedure CustomKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  end;

  { TControladorEdicion }

  TControladorEdicion = class(TLCLComponent)
    procedure AceptarExecute(Sender: TObject);
    procedure CancelarExecute(Sender: TObject);
    procedure EliminarExecute(Sender: TObject);
    procedure GuardarExecute(Sender: TObject);
    procedure SalirExecute(Sender: TObject);
  private
    { Private declarations }
    FAccion: integer;
    FAltaContinua: boolean;
    FAltoBotones: integer;
    FAnchoBotones: integer;
    FBotonAceptar: TBitBtn;
    FBotonCancelar: TBitBtn;
    FControlEncabezado: TDBText;
    FControlInicial: TWinControl;
    FControlMostrarAccion: TLabel;
    FDummyControl: TDummyControl;
    FEsSubform: boolean;
    FIdMaestro: LongInt;
    FIdPadre: LongInt;
    FOldFormActivate: TNotifyEvent;
    FOnValidateForm: TValidateEvent;
    FPanelEdicion: TPanel;
    FParentForm: TForm;
    FQueryGroup: TSQLQueryGroup;
    FStrAccion: string;
    procedure HabilitarControles(TWC: TWinControl; EnableFlag: boolean);
    procedure AsignarKeyDown(TWC: TWinControl);
    procedure SetAltaContinua(AValue: boolean);
    procedure SetAltoBotones(AValue: integer);
    procedure SetAnchoBotones(AValue: integer);
    procedure SetBotonAceptar(AValue: TBitBtn);
    procedure SetBotonCancelar(AValue: TBitBtn);
    procedure SetControlEncabezado(AValue: TDBText);
    procedure SetControlInicial(AValue: TWinControl);
    procedure SetControlMostrarAccion(AValue: TLabel);
    procedure SetEsSubform(AValue: boolean);
    procedure SetIdMaestro(AValue: LongInt);
    procedure SetOnValidateForm(AValue: TValidateEvent);
    procedure SetPanelEdicion(AValue: TPanel);
    procedure SetQueryGroup(AValue: TSQLQueryGroup);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure OnFormActivate(Sender: TObject);
    procedure OnFormCloseQuery(Sender: TObject; var CanClose: boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IdMaestro: LongInt read FIdMaestro write SetIdMaestro;
    property IdPadre: LongInt read FIdPadre;
    property Accion: integer read FAccion;
    property StrAccion: string read FStrAccion;
    procedure nuevoRegistro(IdRegistroPadre: LongInt=-1);
    procedure editarRegistro(id: integer);
    procedure eliminarRegistro(id: integer; MostrarAviso: boolean);
    function NuevoID(tabla: string): longint;
    function ShowModal: TModalResult;
  published
    { Published declarations }
    property AltaContinua: boolean read FAltaContinua write SetAltaContinua default false;
    property AltoBotones: integer read FAltoBotones write SetAltoBotones default 26;
    property AnchoBotones: integer read FAnchoBotones write SetAnchoBotones default 86;
    property BotonAceptar:TBitBtn read FBotonAceptar write SetBotonAceptar;
    property BotonCancelar:TBitBtn read FBotonCancelar write SetBotonCancelar;
    property ControlInicial: TWinControl read FControlInicial write SetControlInicial;
    property ControlMostrarAccion: TLabel read FControlMostrarAccion write SetControlMostrarAccion;
    property ControlEncabezado: TDBText read FControlEncabezado write SetControlEncabezado;
    property EsSubform:boolean read FEsSubform write SetEsSubform default false;
    property OnValidateForm: TValidateEvent read FOnValidateForm write SetOnValidateForm;
    property PanelEdicion: TPanel read FPanelEdicion write SetPanelEdicion;
    property QueryGroup: TSQLQueryGroup read FQueryGroup write SetQueryGroup;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I controladoredicion_icon.lrs}
  RegisterComponents('Mis Componentes',[TControladorEdicion]);
end;

{ TDummyControl }

procedure TDummyControl.CustomKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := VK_TAB;
  end;
  inherited KeyDown(Key, Shift);
end;

{ TControladorEdicion }

procedure TControladorEdicion.AceptarExecute(Sender: TObject);
var
  TodoOK: Boolean;
begin
  if Assigned(FParentForm) then
  begin
    //Realiza la validación del formulario
    TodoOK:=true;
    if Assigned(OnValidateForm) then
      OnValidateForm (Self, TodoOK);
    //Si no hubo errores de validación, continúo, si no no hace nada
    //Se supone que los mensajes de error se emiten en el procedimiento de validación
    if TodoOK then
    begin
      // Si es subform, los cambios se aplican en el form padre
      if not FEsSubform then
      begin
        if Assigned(QueryGroup) then
        begin
          QueryGroup.Post;
          if not QueryGroup.ApplyUpdates(0) then
            MessageDlg('Ocurrió un error al guardar los datos', mtWarning, [mbOK], 0)
          else
            FParentForm.ModalResult := mrOk;
        end;
      end else
        FParentForm.ModalResult := mrOk;
    end;
  end;
end;

procedure TControladorEdicion.CancelarExecute(Sender: TObject);
var
  mr:TModalResult;
begin
  mr:=mrCancel;
  //Antes de cerrar el formulario verifico si hay datos sin guardar y pregunto
  if Assigned(QueryGroup) then
  begin
    QueryGroup.Cancel;
    //Si es un subform no chequeo los cambios pendientes, porque se
    //chequea en el form principal
    if (not EsSubform) and QueryGroup.UpdatesPending then
    begin
      mr:=MessageDlg('Aún quedan datos sin guardar. '+
        '¿Desea guardar estas modificaciones antes de salir?', mtConfirmation,
          mbYesNoCancel, 0);
      if mr=mrYes then //Si responde si, guardo los datos
      begin
        if not QueryGroup.ApplyUpdates(0) then
        begin //Si la grabación dio error, aviso y cancelo la salida
          MessageDlg('Ha ocurrido un error al guardar los datos', mtWarning, [mbOK], 0);
          mr:=mrNone;
        end;
      end else if mr=mrCancel then //Apretó Cancelar, aborto la salida
      begin
        mr:=mrNone;
      end else //Respondió que no, deshago todas las ediciones
      begin
        QueryGroup.CancelUpdates;
        mr:=mrCancel;
      end;
    end;
  end;
  if Assigned(FParentForm) then
  begin
    FParentForm.ModalResult := mr;
  end;
end;

procedure TControladorEdicion.EliminarExecute(Sender: TObject);
var
  mr: TModalResult;
begin
  if Assigned(FParentForm) then
  begin
    mr := MessageDlg('¿Confirma la eliminación de estos datos?', mtWarning, mbYesNo, 0);
    if mr = mrYes then
    begin
      if Assigned(QueryGroup) and Assigned(QueryGroup.QueryMaestro) then
      begin
        QueryGroup.QueryMaestro.Delete;
        // Si es subform, los cambios se aplican en el form padre
        if not FEsSubform then
        begin
          if QueryGroup.ApplyUpdates(0) then
            mr := mrOk
          else
            mr := mrCancel;
        end else;
          mr := mrOk
      end;
    end
    else
      mr := mrNone;
    FParentForm.ModalResult := mr;
  end;
end;

procedure TControladorEdicion.GuardarExecute(Sender: TObject);
var
  TodoOK: boolean;
begin
  if Assigned(FParentForm) then
  begin
    //Realiza la validación del formulario
    TodoOK:=true;
    if Assigned(OnValidateForm) then
      OnValidateForm (Self, TodoOK);
    //Si no hubo errores de validación, continúo, si no no hace nada
    //Se supone que los mensajes de error se emiten en el procedimiento de validación
    if TodoOK then
    begin
      // Si es subform, los cambios se aplican en el form padre
      if not FEsSubform then
      begin
        if Assigned(QueryGroup) then
        begin
          QueryGroup.Post;
          if not QueryGroup.ApplyUpdates(0) then
            MessageDlg('Ocurrió un error al guardar los datos', mtWarning, [mbOK], 0)
          else if AltaContinua then
          begin
            if MessageDlg('¿Desea ingresar otro nuevo dato en este formulario?',
              mtConfirmation,mbYesNo,0) = mrYes then
            begin
              if Assigned(ControlInicial) and ControlInicial.CanFocus then
                ControlInicial.SetFocus;
              nuevoRegistro(FIdPadre);
            end
          end
          else
            FParentForm.ModalResult := mrNone;
        end;
      end
      else if AltaContinua then
      begin
        if MessageDlg('¿Desea ingresar otro nuevo dato en este formulario?',
          mtConfirmation,mbYesNo,0) = mrYes then
        begin
          if Assigned(ControlInicial) and ControlInicial.CanFocus then
            ControlInicial.SetFocus;
          nuevoRegistro(FIdPadre);
        end
        else
          FParentForm.ModalResult:=mrOK;
      end
      else
        FParentForm.ModalResult := mrNone;
    end;
  end;
end;

procedure TControladorEdicion.SalirExecute(Sender: TObject);
begin
  if Assigned (QueryGroup) then
    QueryGroup.Cancel;
  if Assigned(FParentForm) then
  begin
    FParentForm.ModalResult := mrYes;
  end;
end;

procedure TControladorEdicion.OnFormActivate(Sender: TObject);
begin
  if Assigned(FParentForm) then
  begin
    if Assigned (FOldFormActivate) then
      FOldFormActivate(Self);
    //Asigno el comportamiento del KeyDown a los controles del form
    AsignarKeyDown(FParentForm);
    if (FAccion in [ED_AGREGAR, ED_MODIFICAR]) and Assigned(FControlInicial) and
      (FControlInicial.CanFocus) then
    begin
      FControlInicial.SetFocus;
    end
    else if (FAccion = ED_ELIMINAR) and Assigned(FBotonAceptar) and (FBotonAceptar.CanFocus) then
    begin
      FBotonAceptar.SetFocus;
    end;
  end;
end;

procedure TControladorEdicion.OnFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  mr:TModalResult;
begin
  CanClose:=true;
  //Antes de cerrar el formulario verifico si hay datos sin guardar y pregunto
  if Assigned(QueryGroup) then
  begin
    QueryGroup.Post;
    //Si es un subform no chequeo los cambios pendientes, porque se
    //chequea en el form principal
    if (not EsSubform) and QueryGroup.UpdatesPending then
    begin
      mr:=MessageDlg('Aún quedan datos sin guardar. '+
        '¿Desea guardar estas modificaciones antes de salir?', mtConfirmation,
          mbYesNoCancel, 0);
      if mr=mrYes then //Si responde si, guardo los datos
      begin
        if not QueryGroup.ApplyUpdates(0) then
        begin //Si la grabación dio error, aviso y cancelo la salida
          MessageDlg('Ha ocurrido un error al guardar los datos', mtWarning, [mbOK], 0);
          CanClose:=false;
        end;
      end else if mr=mrCancel then //Apretó Cancelar, aborto la salida
      begin
        CanClose:=false;
      end;
    end;
  end;
end;

procedure TControladorEdicion.HabilitarControles(TWC: TWinControl;
  EnableFlag: boolean);
var
  i: integer;
begin
  //Asigno el comportamiento de que el <ENTER> funcione como <TAB> para cambiar
  //de campo en los controles de edición. Se hace en forma recursiva para todos
  //los controles hijos del frame
  //NOTA: No se aplica a los controles con "Tag" <> 0. Esto es para permitir
  //indicar ciertos controles que no sean afectados por este procedimiento
  //por ejemplo, porque la habilitación se determina por otras circunstancias
  with TWC do
  begin
    for i := 0 to ControlCount - 1 do
    begin
      //Llamada recursiva
      if (Controls[i] is TWinControl) and
        ((Controls[i] as TWinControl).ControlCount > 0) then
        HabilitarControles(Controls[i] as TWinControl, EnableFlag);
      if Controls[i].Tag=0 then //Saltea controles con Tag <> 0
        Controls[i].Enabled := EnableFlag;
    end;
  end;
end;

procedure TControladorEdicion.AsignarKeyDown(TWC: TWinControl);
var
  i: integer;
begin
  //Asigno el comportamiento de que el <ENTER> funcione como <TAB> para cambiar
  //de campo en los controles de edición. Se hace en forma recursiva para todos
  //los controles hijos del frame
  with TWC do
  begin
    for i := 0 to ControlCount - 1 do
    begin
      //Llamada recursiva
      if (Controls[i] is TWinControl) and
        ((Controls[i] as TWinControl).ControlCount > 0) then
        AsignarKeyDown(Controls[i] as TWinControl);
      if (Controls[i] is TCustomEdit) then
        (Controls[i] as TCUstomEdit).OnKeyDown := @FDummyControl.CustomKeyDown
      else if (Controls[i] is TCustomComboBox) then
        (Controls[i] as TCustomComboBox).OnKeyDown := @FDummyControl.CustomKeyDown
      else if (Controls[i] is TCustomDateTimePicker) then
        (Controls[i] as TCustomDateTimePicker).OnKeyDown := @FDummyControl.CustomKeyDown
      //Ver cómo es el comportamiento del Calendar para incluirlo o no
      //else if (Controls[i] is TCustomCalendar) then
      //  (Controls[i] as TCustomCalendar).OnKeyDown := @CustomKeyDown
      else if (Controls[i] is TCustomCheckBox) then
        (Controls[i] as TCustomCheckBox).OnKeyDown := @FDummyControl.CustomKeyDown;
    end;
  end;
end;

procedure TControladorEdicion.SetAltaContinua(AValue: boolean);
begin
  if FAltaContinua=AValue then Exit;
  FAltaContinua:=AValue;
end;

procedure TControladorEdicion.SetAltoBotones(AValue: integer);
begin
  if FAltoBotones=AValue then Exit;
  FAltoBotones:=AValue;
  if FBotonAceptar <> nil then
    FBotonAceptar.Height := FAltoBotones;
  if FBotonCancelar <> nil then
    FBotonCancelar.Height := FAltoBotones;
end;

procedure TControladorEdicion.SetAnchoBotones(AValue: integer);
begin
  if FAnchoBotones = AValue then
    exit;
  FAnchoBotones := AValue;
  if FBotonAceptar <> nil then
    FBotonAceptar.Width := FAnchoBotones;
  if FBotonCancelar <> nil then
    FBotonCancelar.Width := FAnchoBotones;
end;

procedure TControladorEdicion.SetBotonAceptar(AValue: TBitBtn);
begin
  if FBotonAceptar=AValue then Exit;
  FBotonAceptar:=AValue;
  if Assigned (FBotonAceptar) then
  begin
    FBotonAceptar.Caption:=BC_ACEPTAR;
    FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_aceptar'));
    FBotonAceptar.Height:=FAltoBotones;
    FBotonAceptar.Width:=FAnchoBotones;
  end;
end;

procedure TControladorEdicion.SetBotonCancelar(AValue: TBitBtn);
begin
  if FBotonCancelar=AValue then Exit;
  FBotonCancelar:=AValue;
  if Assigned (FBotonCancelar) then
  begin
    FBotonCancelar.Caption:=BC_CANCELAR;
    FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
    FBotonCancelar.Height:=FAltoBotones;
    FBotonCancelar.Width:=FAnchoBotones;
  end;
end;

procedure TControladorEdicion.SetControlEncabezado(AValue: TDBText);
begin
  if FControlEncabezado=AValue then Exit;
  FControlEncabezado:=AValue;
end;

procedure TControladorEdicion.SetControlInicial(AValue: TWinControl);
begin
  if FControlInicial=AValue then Exit;
  FControlInicial:=AValue;
end;

procedure TControladorEdicion.SetControlMostrarAccion(AValue: TLabel);
begin
  if FControlMostrarAccion=AValue then Exit;
  FControlMostrarAccion:=AValue;
end;

procedure TControladorEdicion.SetEsSubform(AValue: boolean);
begin
  if FEsSubform=AValue then Exit;
  FEsSubform:=AValue;
end;

procedure TControladorEdicion.SetIdMaestro(AValue: LongInt);
begin
  if FIdMaestro=AValue then Exit;
  FIdMaestro:=AValue;
end;

procedure TControladorEdicion.SetOnValidateForm(AValue: TValidateEvent);
begin
  if FOnValidateForm=AValue then Exit;
  FOnValidateForm:=AValue;
end;

procedure TControladorEdicion.SetPanelEdicion(AValue: TPanel);
begin
  if FPanelEdicion=AValue then Exit;
  FPanelEdicion:=AValue;
end;

procedure TControladorEdicion.SetQueryGroup(AValue: TSQLQueryGroup);
begin
  if FQueryGroup=AValue then Exit;
  FQueryGroup:=AValue;
end;

procedure TControladorEdicion.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FBotonAceptar <> nil) and (AComponent = BotonAceptar) then
      BotonAceptar := nil;
    if (FBotonCancelar <> nil) and (AComponent = BotonCancelar) then
      BotonCancelar := nil;
    if (FControlEncabezado <> nil) and (AComponent = ControlEncabezado) then
      ControlEncabezado := nil;
    if (FControlInicial <> nil) and (AComponent = ControlInicial) then
      ControlInicial := nil;
    if (FControlMostrarAccion <> nil) and (AComponent = ControlMostrarAccion) then
      ControlMostrarAccion := nil;
    if (FPanelEdicion <> nil) and (AComponent = PanelEdicion) then
      PanelEdicion := nil;
    if (FQueryGroup <> nil) and (AComponent = QueryGroup) then
      QueryGroup := nil;
  end;
end;

constructor TControladorEdicion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDummyControl:=TDummyControl.Create(Self);
  if (AOwner is TForm) and not (csDesigning in ComponentState) then
  begin
    FParentForm:=(AOwner as TForm);
    //Asigno el evento OnActivate
    if Assigned(FParentForm.OnActivate) then
      FOldFormActivate:=FParentForm.OnActivate;
    FParentForm.OnActivate:=@OnFormActivate;
    //Asigno el evento OnCloseQuery
    FParentForm.OnCloseQuery:=@OnFormCloseQuery;
  end;
  FAltoBotones:=26;
  FAnchoBotones:=86;
end;

destructor TControladorEdicion.Destroy;
begin
  inherited Destroy;
end;

procedure TControladorEdicion.nuevoRegistro(IdRegistroPadre: LongInt=-1);
begin
  FIdPadre:=IdRegistroPadre;
  FAccion := ED_AGREGAR;
  FStrAccion := 'Nuevo registro';
  if Assigned(FControlMostrarAccion) then
  begin
    FControlMostrarAccion.Caption:=FStrAccion;
    FControlMostrarAccion.Font.Color := clDefault;
  end;

  if Assigned(FQueryGroup) and Assigned(FQueryGroup.QueryMaestro) then
  begin
    //Si es un subform, uso el dataset como está, no tengo que abrir nada
    if not FEsSubform then
    begin
      if FQueryGroup.QueryMaestro.Active then
        FQueryGroup.QueryMaestro.Close;
      //Seteo el parámetro en (-1) para que no traiga registros, y luego agrego uno
      FQueryGroup.QueryMaestro.Params.ParamByName(FQueryGroup.NombreIdMaestro).AsInteger := -1;
      FQueryGroup.QueryMaestro.Prepare;
      FQueryGroup.QueryMaestro.Open;
      //Abro los demás Querys
      FQueryGroup.Close;
      FQueryGroup.Open;
    end;
    //Agrego el registro en blanco
    FQueryGroup.QueryMaestro.Append;
    //Configuro las acciones de los botones según el mode de "Alta continua"
      //Configuro los botones
    if Assigned (FBotonAceptar) then
    begin
      if FAltaContinua then
      begin
        FBotonAceptar.Caption:=BC_GUARDAR;
        FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_guardar'));
        FBotonAceptar.OnClick := @GuardarExecute
      end
      else
      begin
        FBotonAceptar.Caption:=BC_ACEPTAR;
        FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_aceptar'));
        FBotonAceptar.OnClick := @AceptarExecute;
      end;
    end;
    if Assigned (FBotonCancelar) then
    begin
      if FAltaContinua then
      begin
        FBotonCancelar.Caption:=BC_SALIR;
        FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cerrar'));
        FBotonCancelar.OnClick := @SalirExecute
      end
      else
      begin
        FBotonCancelar.Caption:=BC_CANCELAR;
        FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
        FBotonCancelar.OnClick := @CancelarExecute;
      end;
    end;
    //Habilito todos los controles del panel principal
    if Assigned(FPanelEdicion) then;
      HabilitarControles(FPanelEdicion, True);
  end else
    ShowMessage('No se ha indicado la fuente de datos principal');
end;

procedure TControladorEdicion.editarRegistro(id: integer);
begin
  FIdMaestro:=id;
  AltaContinua := False;
  FAccion := ED_MODIFICAR;

  if Assigned(FQueryGroup.QueryMaestro) then
  begin
    //Si es un subform, uso el registro actual, no tengo que abrir nada
    if not FEsSubform then
    begin
      if FQueryGroup.QueryMaestro.Active then
        FQueryGroup.QueryMaestro.Close;
      FQueryGroup.QueryMaestro.Params.ParamByName(FQueryGroup.NombreIdMaestro).AsInteger := id;
      FQueryGroup.QueryMaestro.Prepare;
      FQueryGroup.QueryMaestro.Open;
      //Abro los demás Querys (los cierro por si estaban abiertos)
      FQueryGroup.Close;
      FQueryGroup.Open;
    end;
    FStrAccion := 'Editando';
    if Assigned(FControlMostrarAccion) then
    begin
      FControlMostrarAccion.Caption:=FStrAccion;
      FControlMostrarAccion.Font.Color := clDefault;
    end;
    //Configuro los botones
    if Assigned (FBotonAceptar) then
    begin
      FBotonAceptar.Caption:=BC_ACEPTAR;
      FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_aceptar'));
      FBotonAceptar.OnClick := @AceptarExecute;
    end;
    if Assigned (FBotonCancelar) then
    begin
      FBotonCancelar.Caption:=BC_CANCELAR;
      FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
      FBotonCancelar.OnClick := @CancelarExecute;
    end;
    //Habilito todos los controles del panel principal
    if Assigned(FPanelEdicion) then
      HabilitarControles(FPanelEdicion, True);
  end else
    ShowMessage('No se ha indicado la fuente de datos principal');
end;

procedure TControladorEdicion.eliminarRegistro(id: integer; MostrarAviso: boolean
  );
begin
  FAltaContinua := False;
  FIdMaestro:=id;
  FAccion := ED_ELIMINAR;
  if not FEsSubform then
    FQueryGroup.Open;

  if Assigned(FQueryGroup.QueryMaestro) then
  begin
    if not FEsSubform then
    begin
      if FQueryGroup.QueryMaestro.Active then
        FQueryGroup.QueryMaestro.Close;
      FQueryGroup.QueryMaestro.Params.ParamByName(FQueryGroup.NombreIdMaestro).AsInteger := id;
      FQueryGroup.QueryMaestro.Prepare;
      FQueryGroup.QueryMaestro.Open;
      //Abro los demás Querys
      FQueryGroup.Close;
      FQueryGroup.Open;
    end;
    FStrAccion := 'Eliminando';
    if Assigned(FControlMostrarAccion) then
    begin
      FControlMostrarAccion.Caption:=FStrAccion;
      FControlMostrarAccion.Font.Color := clRed;
    end;
    //Configuro los botones
    if Assigned (FBotonAceptar) then
    begin
      FBotonAceptar.Caption:=BC_ELIMINAR;
      FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_eliminar'));
      FBotonAceptar.OnClick := @EliminarExecute;
    end;
    if Assigned (FBotonCancelar) then
    begin
      FBotonCancelar.Caption:=BC_CANCELAR;
      FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
      FBotonCancelar.OnClick := @CancelarExecute;
    end;
    //Deshabilito todos los controles del panel principal
    if Assigned(FPanelEdicion) then;
      HabilitarControles(FPanelEdicion, False);
    //Aviso al usuario que se está por eliminar
    if MostrarAviso then
    begin
      MessageDlg('Ha solicitado eliminar los datos mostrados. Luego de cerrar este ' +
        'mensaje, si está de acuerdo, confirme la operación presionando el botón ' +
        '"Eliminar" del formulario.', mtWarning, [mbClose], 0);
    end;
  end else
    ShowMessage('No se ha indicado la fuente de datos principal');
end;

function TControladorEdicion.NuevoID(tabla: string): longint;
var
  ID: integer;
  sq:TSQLQuery;
begin
  ID := -1;
  if tabla <> '' then
  begin
    //Creo un SQLQuery para obtener el dato desde la base de datos
    sq:=TSQLQuery.Create(Self);
    try
      sq.DataBase:=FQueryGroup.Database;
      sq.SQL.Clear;
      sq.SQL.Add('SELECT nuevo_id(''' + tabla + ''') as ID');
      sq.Open;
      if sq.RecordCount = 1 then
      begin
        ID := sq.FieldByName('ID').AsInteger;
        if ID < 1 then
          ID := -1;
      end;
    finally
      sq.Free;
    end;
  end;
  Result := ID;
end;

function TControladorEdicion.ShowModal: TModalResult;
begin
  if Assigned(FParentForm) then
    Result := FParentForm.ShowModal
  else
    Result:=mrNone;
end;

initialization
{$I bt_controladoredicion.lrs}
{$I mis_componentes.lrs}
end.
