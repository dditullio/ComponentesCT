unit zcontroladoredicion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,LCLClasses,
  Buttons, ZDataset, ZDatasetGroup, StdCtrls, DbCtrls, ExtCtrls,LCLType, DateTimePicker,
  ActnList, ZStoredProcedure, db, Grids;
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

  { TZControladorEdicion }

  TZControladorEdicion = class(TLCLComponent)
    procedure AceptarExecute(Sender: TObject);
    procedure CancelarExecute(Sender: TObject);
    procedure EliminarExecute(Sender: TObject);
    procedure GuardarExecute(Sender: TObject);
    procedure SalirExecute(Sender: TObject);
  private
    { Private declarations }
    FAccion: integer;
    //Acciones para los botones
    FacAceptar:TAction;
    FacCancelar:TAction;
    FacEliminar:TAction;
    FacGuardar:TAction;
    FacSalir:TAction;
    FAltaContinua: boolean;
    FAltaContinua_ORIG: boolean;
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
    FOnInitRecord: TNotifyEvent;
    FOnValidateForm: TValidateEvent;
    FPanelEdicion: TPanel;
    FParentForm: TForm;
    FZDatasetGroup: TZDatasetGroup;
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
    procedure SetPanelEdicion(AValue: TPanel);
    procedure SetZDatasetGroup(AValue: TZDatasetGroup);
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
    function Aceptar: boolean;
    function Guardar: boolean;
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
    property OnValidateForm: TValidateEvent read FOnValidateForm write FOnValidateForm;
    property OnInitRecord: TNotifyEvent read FOnInitRecord write FOnInitRecord;
    property PanelEdicion: TPanel read FPanelEdicion write SetPanelEdicion;
    property ZDatasetGroup: TZDatasetGroup read FZDatasetGroup write SetZDatasetGroup;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I controladoredicion_icon.lrs}
  RegisterComponents('Mis Componentes',[TZControladorEdicion]);
end;

{ TDummyControl }

procedure TDummyControl.CustomKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Key := VK_TAB;
  // Ver como hacer para que la flecha arriba vaya un control hacia atrás
  end
  else if ((Sender is TWinControl)  and not ((Sender is TCustomMemo) or (Sender is TCustomGrid) or (Sender is TCustomListBox) or (Sender is TCustomComboBox) or (Sender is TCustomDateTimePicker))) then
  begin
       if (Key = VK_UP) then
              (Sender as TWinControl).PerformTab(false);
       if (Key = VK_DOWN) then
              (Sender as TWinControl).PerformTab(true);
  end;
  inherited KeyDown(Key, Shift);
end;

{ TZControladorEdicion }

procedure TZControladorEdicion.AceptarExecute(Sender: TObject);
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
        if Assigned(ZDatasetGroup) then
        begin
          ZDatasetGroup.Post;
          if not ZDatasetGroup.ApplyUpdates then
          begin
            //Ya se emite el error en el ZDatesetGroup, por eso se saca de acá
            //MessageDlg('Ocurrió un error al guardar los datos', mtWarning, [mbOK], 0)
            FParentForm.ModalResult:=mrNone;
          end
          else
            FParentForm.ModalResult := mrOk;
        end;
      end else
        FParentForm.ModalResult := mrOk;
    end;
  end;
end;

procedure TZControladorEdicion.CancelarExecute(Sender: TObject);
var
  mr:TModalResult;
begin
  mr:=mrCancel;
  //Antes de cerrar el formulario verifico si hay datos sin guardar y pregunto
  if Assigned(ZDatasetGroup) then
  begin
    //ZDatasetGroup.Cancel; //Si cancela, pierde los cambios sin preguntar

    //Si es un subform no chequeo los cambios pendientes, porque se
    //chequea en el form principal
    if (not EsSubform) and ZDatasetGroup.UpdatesPending then
    begin
      mr:=MessageDlg('Aún quedan datos sin guardar. '+
        '¿Desea guardar estas modificaciones antes de salir?', mtConfirmation,
          mbYesNoCancel, 0);
      if mr=mrYes then //Si responde si, guardo los datos
      begin
        if not ZDatasetGroup.ApplyUpdates then
        begin //Si la grabación dio error, aviso y cancelo la salida
          MessageDlg('Ha ocurrido un error al guardar los datos', mtWarning, [mbOK], 0);
          mr:=mrNone;
        end;
      end else if mr=mrCancel then //Apretó Cancelar, aborto la salida
      begin
        mr:=mrNone;
      end else //Respondió que no, deshago todas las ediciones
      begin
        ZDatasetGroup.CancelUpdates;
        mr:=mrCancel;
      end;
    end;
  end;
  if Assigned(FParentForm) then
  begin
    FParentForm.ModalResult := mr;
  end;
end;

procedure TZControladorEdicion.EliminarExecute(Sender: TObject);
var
  mr: TModalResult;
begin
  if Assigned(FParentForm) then
  begin
    mr := MessageDlg('¿Confirma la eliminación de estos datos?', mtWarning, mbYesNo, 0);
    if mr = mrYes then
    begin
      if Assigned(ZDatasetGroup) and Assigned(ZDatasetGroup.DatasetMaestro) then
      begin
        ZDatasetGroup.DatasetMaestro.Delete;
        // Si es subform, los cambios se aplican en el form padre
        if not FEsSubform then
        begin
          if ZDatasetGroup.ApplyUpdates then
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

procedure TZControladorEdicion.GuardarExecute(Sender: TObject);
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
        if Assigned(ZDatasetGroup) then
        begin
          ZDatasetGroup.Post;
          if not ZDatasetGroup.ApplyUpdates then
            MessageDlg('Ocurrió un error al guardar los datos', mtWarning, [mbOK], 0)
          else if FAltaContinua then
          begin
            if MessageDlg('¿Desea ingresar otro nuevo dato en este formulario?',
              mtConfirmation,mbYesNo,0) = mrYes then
            begin
              if Assigned(ControlInicial) and ControlInicial.CanFocus then
                ControlInicial.SetFocus;
              nuevoRegistro(FIdPadre);
            end else
            begin
              FParentForm.ModalResult := mrOk;
            end;
          end
          else
            FParentForm.ModalResult := mrNone;
        end;
      end
      else if FAltaContinua then
      begin
        ZDatasetGroup.Post;
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

procedure TZControladorEdicion.SalirExecute(Sender: TObject);
begin
  if Assigned (ZDatasetGroup) then
    ZDatasetGroup.Cancel;
  if Assigned(FParentForm) then
  begin
    FParentForm.ModalResult := mrYes;
  end;
end;

procedure TZControladorEdicion.OnFormActivate(Sender: TObject);
begin
  if Assigned(FParentForm) then
  begin
    if Assigned (FOldFormActivate) then
      FOldFormActivate(Self);
    //Asigno el comportamiento del KeyDown a los controles del form
    if not (csDesigning in ComponentState) then
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

procedure TZControladorEdicion.OnFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  mr:TModalResult;
begin
  CanClose:=true;
  //Antes de cerrar el formulario verifico si hay datos sin guardar y pregunto
  if Assigned(ZDatasetGroup) then
  begin
    ZDatasetGroup.Post;
    //Si es un subform no chequeo los cambios pendientes, porque se
    //chequea en el form principal
    if (not EsSubform) and ZDatasetGroup.UpdatesPending then
    begin
      mr:=MessageDlg('Aún quedan datos sin guardar. '+
        '¿Desea guardar estas modificaciones antes de salir?', mtConfirmation,
          mbYesNoCancel, 0);
      if mr=mrYes then //Si responde si, guardo los datos
      begin
        if not ZDatasetGroup.ApplyUpdates then
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

procedure TZControladorEdicion.HabilitarControles(TWC: TWinControl;
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

procedure TZControladorEdicion.AsignarKeyDown(TWC: TWinControl);
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
      // No se hace la llamada recursiva en el Grid para que no cambie el
      // comportamiento en la edición de columnas
      if (Controls[i] is TWinControl) and
        ((Controls[i] as TWinControl).ControlCount > 0) and
        not (Controls[i] is TCustomGrid) then
        AsignarKeyDown(Controls[i] as TWinControl);
      if (Controls[i] is TCustomEdit) then
      (Controls[i] as TCustomEdit).OnKeyDown := @FDummyControl.CustomKeyDown
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

procedure TZControladorEdicion.SetAltaContinua(AValue: boolean);
begin
  if FAltaContinua=AValue then Exit;
  FAltaContinua:=AValue;
  FAltaContinua_ORIG:=AValue;
end;

procedure TZControladorEdicion.SetAltoBotones(AValue: integer);
begin
  if FAltoBotones=AValue then Exit;
  FAltoBotones:=AValue;
  if FBotonAceptar <> nil then
    FBotonAceptar.Height := FAltoBotones;
  if FBotonCancelar <> nil then
    FBotonCancelar.Height := FAltoBotones;
end;

procedure TZControladorEdicion.SetAnchoBotones(AValue: integer);
begin
  if FAnchoBotones = AValue then
    exit;
  FAnchoBotones := AValue;
  if FBotonAceptar <> nil then
    FBotonAceptar.Width := FAnchoBotones;
  if FBotonCancelar <> nil then
    FBotonCancelar.Width := FAnchoBotones;
end;

procedure TZControladorEdicion.SetBotonAceptar(AValue: TBitBtn);
begin
  if FBotonAceptar=AValue then Exit;
  FBotonAceptar:=AValue;
  if Assigned (FBotonAceptar) then
  begin
    FBotonAceptar.Action:=FacAceptar;
    FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_aceptar'));
    FBotonAceptar.Height:=FAltoBotones;
    FBotonAceptar.Width:=FAnchoBotones;
  end;
end;

procedure TZControladorEdicion.SetBotonCancelar(AValue: TBitBtn);
begin
  if FBotonCancelar=AValue then Exit;
  FBotonCancelar:=AValue;
  if Assigned (FBotonCancelar) then
  begin
    FBotonCancelar.Kind:=bkCancel;
    FBotonCancelar.Action:=FacCancelar;
    FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
    FBotonCancelar.Height:=FAltoBotones;
    FBotonCancelar.Width:=FAnchoBotones;
  end;
end;

procedure TZControladorEdicion.SetControlEncabezado(AValue: TDBText);
begin
  if FControlEncabezado=AValue then Exit;
  FControlEncabezado:=AValue;
end;

procedure TZControladorEdicion.SetControlInicial(AValue: TWinControl);
begin
  if FControlInicial=AValue then Exit;
  FControlInicial:=AValue;
end;

procedure TZControladorEdicion.SetControlMostrarAccion(AValue: TLabel);
begin
  if FControlMostrarAccion=AValue then Exit;
  FControlMostrarAccion:=AValue;
end;

procedure TZControladorEdicion.SetEsSubform(AValue: boolean);
begin
  if FEsSubform=AValue then Exit;
  FEsSubform:=AValue;
end;

procedure TZControladorEdicion.SetIdMaestro(AValue: LongInt);
begin
  if FIdMaestro=AValue then Exit;
  FIdMaestro:=AValue;
end;

procedure TZControladorEdicion.SetPanelEdicion(AValue: TPanel);
begin
  if FPanelEdicion=AValue then Exit;
  FPanelEdicion:=AValue;
end;

procedure TZControladorEdicion.SetZDatasetGroup(AValue: TZDatasetGroup);
begin
  if FZDatasetGroup=AValue then Exit;
  FZDatasetGroup:=AValue;
end;

procedure TZControladorEdicion.Notification(AComponent: TComponent;
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
    if (FZDatasetGroup <> nil) and (AComponent = ZDatasetGroup) then
      ZDatasetGroup := nil;
  end;
end;

constructor TZControladorEdicion.Create(AOwner: TComponent);
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
  FAltoBotones:=28;
  FAnchoBotones:=88;

  //Creo las acciones
  //FacAceptar:TAction;
  //FacCancelar:TAction;
  //FacEliminar:TAction;
  //FacGuardar:TAction;
  //FacSalir:TAction;

  // Aceptar:
  FacAceptar:=TAction.Create(self);
  FacAceptar.Visible := True;
  FacAceptar.Enabled := True;
  FacAceptar.Caption := BC_ACEPTAR;
  FacAceptar.OnExecute := @AceptarExecute;
  FacAceptar.ShortCut:=16455; //'Ctrl+G';
  // Cancelar:
  FacCancelar:=TAction.Create(self);
  FacCancelar.Visible := True;
  FacCancelar.Enabled := True;
  FacCancelar.Caption := BC_CANCELAR;
  FacCancelar.OnExecute := @CancelarExecute;

  // Eliminar:
  FacEliminar:=TAction.Create(self);
  FacEliminar.Visible := True;
  FacEliminar.Enabled := True;
  FacEliminar.Caption := BC_ELIMINAR;
  FacEliminar.OnExecute := @EliminarExecute;

  // Guardar:
  FacGuardar:=TAction.Create(self);
  FacGuardar.Visible := True;
  FacGuardar.Enabled := True;
  FacGuardar.Caption := BC_GUARDAR;
  FacGuardar.OnExecute := @GuardarExecute;
  FacGuardar.ShortCut:=16455; //'Ctrl+G';

  // Salir:
  FacSalir:=TAction.Create(self);
  FacSalir.Visible := True;
  FacSalir.Enabled := True;
  FacSalir.Caption := BC_SALIR;
  FacSalir.OnExecute := @SalirExecute;

end;

destructor TZControladorEdicion.Destroy;
begin
  inherited Destroy;
end;

procedure TZControladorEdicion.nuevoRegistro(IdRegistroPadre: LongInt=-1);
begin
  // OJO!!! Falta implementar que setee el valor de la tabla maestra
  // si es un master-detail
  FAltaContinua:=FAltaContinua_ORIG;
  FIdPadre:=IdRegistroPadre;
  FAccion := ED_AGREGAR;
  FStrAccion := 'Nuevo registro';
  if Assigned(FControlMostrarAccion) then
  begin
    FControlMostrarAccion.Caption:=FStrAccion;
    FControlMostrarAccion.Font.Color := clDefault;
  end;

  if Assigned(FZDatasetGroup) and Assigned(FZDatasetGroup.DatasetMaestro) then
  begin
    //Si es un subform, uso el dataset como está, no tengo que abrir nada
    if not FEsSubform then
    begin
      //Abro los Querys complementarios. Los abro primero porque generalmente son
      //lookups que dan valores al dataset maestro
      FZDatasetGroup.Close;
      FZDatasetGroup.OpenReadOnly;
      if FZDatasetGroup.DatasetMaestro.Active then
        FZDatasetGroup.DatasetMaestro.Close;
      //Seteo el parámetro en (-1) para que no traiga registros, y luego agrego uno
      FZDatasetGroup.DatasetMaestro.ParamByName(FZDatasetGroup.CampoIdMaestro).AsInteger := -1;
      FZDatasetGroup.DatasetMaestro.Prepare;
      FZDatasetGroup.DatasetMaestro.Open;
      //Abro los Querys complementarios que se actualizan
      FZDatasetGroup.OpenUpdatable;
    end;
    //Agrego el registro en blanco
    FZDatasetGroup.DatasetMaestro.Append;
    //Configuro las acciones de los botones según el mode de "Alta continua"
      //Configuro los botones
    if Assigned (FBotonAceptar) then
    begin
      if FAltaContinua then
      begin
        FBotonAceptar.Action:=FacGuardar;
        FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_guardar'));
      end
      else
      begin
        FBotonAceptar.Action:=FacAceptar;
        FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_aceptar'));
      end;
    end;
    if Assigned (FBotonCancelar) then
    begin
      if FAltaContinua then
      begin
        FBotonCancelar.Action:=FacSalir;
        FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cerrar'));
      end
      else
      begin
        FBotonCancelar.Action:=FacCancelar;
        FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
      end;
    end;
    //Habilito todos los controles del panel principal
    if Assigned(FPanelEdicion) then;
      HabilitarControles(FPanelEdicion, True);
  end else
    ShowMessage('No se ha indicado la fuente de datos principal');
  if Assigned (FOnInitRecord) then
     FOnInitRecord(Self);
end;

procedure TZControladorEdicion.editarRegistro(id: integer);
begin
  FIdMaestro:=id;
  FAltaContinua := False;
  FAccion := ED_MODIFICAR;

  if Assigned(FZDatasetGroup.DatasetMaestro) then
  begin
    //Si es un subform, uso el registro actual, no tengo que abrir nada
    if not FEsSubform then
    begin
      if FZDatasetGroup.DatasetMaestro.Active then
        FZDatasetGroup.DatasetMaestro.Close;
      FZDatasetGroup.DatasetMaestro.ParamByName(FZDatasetGroup.CampoIdMaestro).AsInteger := id;
      FZDatasetGroup.DatasetMaestro.Open;
      //Abro los demás Querys (los cierro por si estaban abiertos)
      FZDatasetGroup.Close;
      FZDatasetGroup.Open;
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
      FBotonAceptar.Action:=FacAceptar;
      FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_aceptar'));
    end;
    if Assigned (FBotonCancelar) then
    begin
      FBotonCancelar.Action:=FacCancelar;
      FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
    end;
    //Habilito todos los controles del panel principal
    if Assigned(FPanelEdicion) then
      HabilitarControles(FPanelEdicion, True);
  end else
    ShowMessage('No se ha indicado la fuente de datos principal');
  if Assigned (FOnInitRecord) then
     FOnInitRecord(Self);
end;

procedure TZControladorEdicion.eliminarRegistro(id: integer; MostrarAviso: boolean
  );
begin
  FAltaContinua := False;
  FIdMaestro:=id;
  FAccion := ED_ELIMINAR;
  if not FEsSubform then
    FZDatasetGroup.Open;

  if Assigned(FZDatasetGroup.DatasetMaestro) then
  begin
    if not FEsSubform then
    begin
      if FZDatasetGroup.DatasetMaestro.Active then
        FZDatasetGroup.DatasetMaestro.Close;
      FZDatasetGroup.DatasetMaestro.ParamByName(FZDatasetGroup.CampoIdMaestro).AsInteger := id;
//      FZDatasetGroup.DatasetMaestro.Prepare;
      FZDatasetGroup.DatasetMaestro.Open;
      //Abro los demás Querys
      FZDatasetGroup.Close;
      FZDatasetGroup.Open;
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
      FBotonAceptar.Action:=FacEliminar;
      FBotonAceptar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_eliminar'));
    end;
    if Assigned (FBotonCancelar) then
    begin
      FBotonCancelar.Action:=FacCancelar;
      FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btce_cancelar'));
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

function TZControladorEdicion.NuevoID(tabla: string): longint;
var
  ID: integer;
  zsp: TZStoredProc;
begin
  ID := -1;
  if tabla <> '' then
  begin
    //Creo un SQLQuery para obtener el dato desde la base de datos
    zsp:=TZStoredProc.Create(Self);
    try
      zsp.Connection:=FZDatasetGroup.Connection;
      zsp.StoredProcName:='sp_nuevo_id';
      zsp.Params.Add;
      zsp.Params.Add;
      zsp.Params[0].Name:='p_nombre_tabla';
      zsp.Params[0].DataType:=ftString;
      zsp.Params[0].ParamType:=ptInput;
      zsp.Params[1].Name:='p_id';
      zsp.Params[1].DataType:=ftInteger;
      zsp.Params[1].ParamType:=ptOutput;
      zsp.Params[0].AsString:=tabla;
      zsp.Prepare;
      zsp.ExecProc;
      ID := zsp.Params[1].AsInteger;
      if ID < 1 then
        ID := -1;
    finally
      zsp.Free;
    end;
  end;
  Result := ID;
end;

function TZControladorEdicion.ShowModal: TModalResult;
begin
  if Assigned(FParentForm) then
    Result := FParentForm.ShowModal
  else
    Result:=mrNone;
end;

function TZControladorEdicion.Aceptar: boolean;
begin
  if FacAceptar.Enabled then
     Result := FacAceptar.Execute
  else
    Result:=False;
end;

function TZControladorEdicion.Guardar: boolean;
begin
  if FacGuardar.Enabled then
     Result := FacGuardar.Execute
  else
    Result:=False;
end;

initialization
{$I bt_controladoredicion.lrs}
{$I mis_componentes.lrs}
end.
