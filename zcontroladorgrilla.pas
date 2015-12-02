unit zcontroladorgrilla;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, LResources, Graphics, DBGrids, StdCtrls,
  DB, ZDataset, ZAbstractDataset, Forms, Controls, LCLType,
  zcontroladoredicion, rxdbgrid, RxDBGridExportSpreadSheet, ActnList,
  Dialogs, LazFileUtils;

const

  CAD_FILTRO = '__FILTRO__';

type

  TTipoBuscador = (
    tbParametro, //Busca registro utilizando el parámetro __FILTRO__ del query
    tbFiltro,
    //Usa la propiedad "Filter" del Query (en subforms, donde no se puede cerrar y abrir el query)
    tbBusqueda
    //Usa el método "Locate" del Query. No filtra, sólo ubica el registro (en subforms, donde no se puede cerrar y abrir el query)
    );

  { TZControladorGrilla }

  TZControladorGrilla = class(TComponent)
  private
    FAntesBuscar: TNotifyEvent;
    FAntesEliminar: TNotifyEvent;
    FAntesEditar: TNotifyEvent;
    FAntesAgregar: TNotifyEvent;
    FAvisarBorrado: boolean;
    FBotonBuscar: TBitBtn;
    FBotonAgregar: TBitBtn;
    FBotonEditar: TBitBtn;
    FBotonEliminar: TBitBtn;
    FBotonExportar: TBitBtn;
    FBotonSeleccionar: TBitBtn;
    FBotonCancelar: TBitBtn;
    FBotonCerrar: TBitBtn;
    FAltoBotones: integer;
    FAnchoBotones: integer;
    FBuscador: TCustomEdit;
    FCampoIdPadre: TNumericField;
    FControlEdicion: TZControladorEdicion;
    FDespuesBuscar: TNotifyEvent;
    FDespuesEliminar: TNotifyEvent;
    FDespuesEditar: TNotifyEvent;
    FDespuesAgregar: TNotifyEvent;
    FExporting: boolean;
    FExpresionFiltro: string;
    FGrilla: TCustomDBGrid;
    FMostrarBotonExportar: boolean;
    FSeleccionarUltimo: Boolean;
    FZDataset: TZAbstractDataset;
    FTipoBuscador: TTipoBuscador;
    //Acciones para los botones
    FalBotones: TActionList;
    FacAgregar: TAction;
    FacAtras: TAction;
    FacCancelar: TAction;
    FacEliminar: TAction;
    FacEditar: TAction;
    FacExportar: TAction;
    FacSalir: TAction;
    FacAceptar: TAction;
    FacCerrar: TAction;
    FacBuscar: TAction;
    FacSeleccionar: TAction;
    FExportGrid: TRxDBGridExportSpreadSheet;
    FFileSaveDialog: TSaveDialog;
    //Variables para guardar eventos definidos por el usuario
    FOldGrillaKeyDown: TKeyEvent;
    FOldGrillaDblClick: TNotifyEvent;
    FOldBuscadorKeyDown: TKeyEvent;
    procedure SetAltoBotones(const AValue: integer);
    procedure SetAnchoBotones(const AValue: integer);
    procedure SetAntesBuscar(AValue: TNotifyEvent);
    procedure SetAntesEliminar(AValue: TNotifyEvent);
    procedure SetAntesEditar(AValue: TNotifyEvent);
    procedure SetAntesAgregar(AValue: TNotifyEvent);
    procedure SetAvisarBorrado(AValue: boolean);
    procedure SetBotonBuscar(const AValue: TBitBtn);
    procedure SetBotonAgregar(const AValue: TBitBtn);
    procedure SetBotonEditar(const AValue: TBitBtn);
    procedure SetBotonEliminar(const AValue: TBitBtn);
    procedure SetBotonExportar(AValue: TBitBtn);
    procedure SetBotonSeleccionar(const AValue: TBitBtn);
    procedure SetBotonCancelar(const AValue: TBitBtn);
    procedure SetBotonCerrar(const AValue: TBitBtn);
    procedure SetBuscador(const AValue: TCustomEdit);
    procedure SetCampoIdPadre(AValue: TNumericField);
    procedure SetControlEdicion(AValue: TZControladorEdicion);
    procedure SetDespuesBuscar(AValue: TNotifyEvent);
    procedure SetDespuesEliminar(AValue: TNotifyEvent);
    procedure SetDespuesEditar(AValue: TNotifyEvent);
    procedure SetDespuesAgregar(AValue: TNotifyEvent);
    procedure SetExpresionFiltro(AValue: string);
    //Tipo: Utilizar "__FILTRO__" dentro de la cadena para armar la expresión a buscar
    //Por ejemplo: "Nombre=__FILTRO__ OR Descripcion=__FILTRO__"
    procedure SetGrilla(const AValue: TCustomDBGrid);
    procedure SetMostrarBotonExportar(AValue: boolean);
    procedure SetSeleccionarUltimo(AValue: Boolean);
    procedure SetZDataset(const AValue: TZAbstractDataset);

    procedure AgregarExecute(Sender: TObject);
    procedure BuscarExecute(Sender: TObject);
    procedure CancelarExecute(Sender: TObject);
    procedure EliminarExecute(Sender: TObject);
    procedure ExportarExecute(Sender: TObject);
    procedure EditarExecute(Sender: TObject);
    procedure SeleccionarExecute(Sender: TObject);
    procedure GrillaDblClick(Sender: TObject);
    procedure QueryBeforeOpen(DataSet: TDataSet);
    procedure QueryAfterOpen(DataSet: TDataSet);
    procedure edBuscarKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure GrillaKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SetTipoBuscador(AValue: TTipoBuscador);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure HabilitarAcciones;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure refrescarQuery(enfocarBuscador: boolean = False);
    property Exporting: boolean read FExporting;
  published
    property AltoBotones: integer read FAltoBotones write SetAltoBotones default 26;
    property AnchoBotones: integer read FAnchoBotones write SetAnchoBotones default 86;
    property AntesAgregar: TNotifyEvent read FAntesAgregar write SetAntesAgregar;
    property AntesBuscar: TNotifyEvent read FAntesBuscar write SetAntesBuscar;
    property AntesEditar: TNotifyEvent read FAntesEditar write SetAntesEditar;
    property AntesEliminar: TNotifyEvent read FAntesEliminar write SetAntesEliminar;
    property AvisarBorrado: boolean
      read FAvisarBorrado write SetAvisarBorrado default True;
    property BotonAgregar: TBitBtn read FBotonAgregar write SetBotonAgregar;
    property BotonEditar: TBitBtn read FBotonEditar write SetBotonEditar;
    property BotonExportar: TBitBtn read FBotonExportar write SetBotonExportar;
    property BotonEliminar: TBitBtn read FBotonEliminar write SetBotonEliminar;
    property BotonSeleccionar: TBitBtn read FBotonSeleccionar write SetBotonSeleccionar;
    property BotonCancelar: TBitBtn read FBotonCancelar write SetBotonCancelar;
    property BotonCerrar: TBitBtn read FBotonCerrar write SetBotonCerrar;
    property BotonBuscar: TBitBtn read FBotonBuscar write SetBotonBuscar;
    property Buscador: TCustomEdit read FBuscador write SetBuscador;
    property SeleccionarUltimo: Boolean read FSeleccionarUltimo write SetSeleccionarUltimo;
    property CampoIdPadre: TNumericField read FCampoIdPadre write SetCampoIdPadre;
    property ControlEdicion: TZControladorEdicion
      read FControlEdicion write SetControlEdicion;
    property DespuesAgregar: TNotifyEvent read FDespuesAgregar write SetDespuesAgregar;
    property DespuesBuscar: TNotifyEvent read FDespuesBuscar write SetDespuesBuscar;
    property DespuesEditar: TNotifyEvent read FDespuesEditar write SetDespuesEditar;
    property DespuesEliminar: TNotifyEvent read FDespuesEliminar
      write SetDespuesEliminar;
    property ExpresionFiltro: string read FExpresionFiltro write SetExpresionFiltro;
    property Grilla: TCustomDBGrid read FGrilla write SetGrilla;
    property MostrarBotonExportar: boolean read FMostrarBotonExportar
      write SetMostrarBotonExportar;
    property ZDataset: TZAbstractDataset read FZDataset write SetZDataset;
    property TipoBuscador: TTipoBuscador
      read FTipoBuscador write SetTipoBuscador default tbParametro;
    procedure Buscar;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mis Componentes', [TZControladorGrilla]);
end;

{ TZControladorGrilla }

procedure TZControladorGrilla.SetBotonAgregar(const AValue: TBitBtn);
begin
  if FBotonAgregar = AValue then
    exit;
  FBotonAgregar := AValue;
  if FBotonAgregar <> nil then
  begin
    FBotonAgregar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_add'));
    FBotonAgregar.Width := FAnchoBotones;
    FBotonAgregar.Height := FAltoBotones;
    FBotonAgregar.Action := FacAgregar;
  end;
end;

procedure TZControladorGrilla.SetAnchoBotones(const AValue: integer);
begin
  if FAnchoBotones = AValue then
    exit;
  FAnchoBotones := AValue;
  if FBotonAgregar <> nil then
    FBotonAgregar.Width := FAnchoBotones;
  if FBotonEditar <> nil then
    FBotonEditar.Width := FAnchoBotones;
  if FBotonEliminar <> nil then
    FBotonEliminar.Width := FAnchoBotones;
  if FBotonSeleccionar <> nil then
    FBotonSeleccionar.Width := FAnchoBotones;
  if FBotonCancelar <> nil then
    FBotonCancelar.Width := FAnchoBotones;
  if FBotonCerrar <> nil then
    FBotonCerrar.Width := FAnchoBotones;
  //Paso los valores al control de edicion
  if Assigned(ControlEdicion) then
  begin
    ControlEdicion.AnchoBotones := FAnchoBotones;
  end;
end;

procedure TZControladorGrilla.SetAntesBuscar(AValue: TNotifyEvent);
begin
  if FAntesBuscar = AValue then
    Exit;
  FAntesBuscar := AValue;
end;

procedure TZControladorGrilla.SetAntesEliminar(AValue: TNotifyEvent);
begin
  if FAntesEliminar = AValue then
    Exit;
  FAntesEliminar := AValue;
end;

procedure TZControladorGrilla.SetAntesEditar(AValue: TNotifyEvent);
begin
  if FAntesEditar = AValue then
    Exit;
  FAntesEditar := AValue;
end;

procedure TZControladorGrilla.SetAntesAgregar(AValue: TNotifyEvent);
begin
  if FAntesAgregar = AValue then
    Exit;
  FAntesAgregar := AValue;
end;

procedure TZControladorGrilla.SetAvisarBorrado(AValue: boolean);
begin
  if FAvisarBorrado = AValue then
    Exit;
  FAvisarBorrado := AValue;
end;

procedure TZControladorGrilla.SetBotonBuscar(const AValue: TBitBtn);
begin
  if FBotonBuscar = AValue then
    exit;
  FBotonBuscar := AValue;
  if FBotonBuscar <> nil then
  begin
    FBotonBuscar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_search'));
    FBotonBuscar.Action := FacBuscar;
    FBotonBuscar.Caption := '';
    //Botón PEQUEÑO, se asigna el alto predeterminado del TEdit y un ancho un poco
    //mayor para que el Glyph se vea bien
    FBotonBuscar.Width := 28;
    FBotonBuscar.Height := 25;
    FBotonBuscar.Layout := blGlyphTop;
  end;
end;

procedure TZControladorGrilla.SetAltoBotones(const AValue: integer);
begin
  if FAltoBotones = AValue then
    exit;
  FAltoBotones := AValue;
  if FBotonAgregar <> nil then
    FBotonAgregar.Height := FAltoBotones;
  if FBotonEditar <> nil then
    FBotonEditar.Height := FAltoBotones;
  if FBotonEliminar <> nil then
    FBotonEliminar.Height := FAltoBotones;
  if FBotonSeleccionar <> nil then
    FBotonSeleccionar.Height := FAltoBotones;
  if FBotonCancelar <> nil then
    FBotonCancelar.Height := FAltoBotones;
  if FBotonCerrar <> nil then
    FBotonCerrar.Height := FAltoBotones;
  //Paso los valores al control de edicion
  if Assigned(ControlEdicion) then
  begin
    ControlEdicion.AltoBotones := FAltoBotones;
  end;
end;

procedure TZControladorGrilla.SetBotonEditar(const AValue: TBitBtn);
begin
  if FBotonEditar = AValue then
    exit;
  FBotonEditar := AValue;
  if FBotonEditar <> nil then
  begin
    FBotonEditar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_edit'));
    FBotonEditar.Width := FAnchoBotones;
    FBotonEditar.Height := FAltoBotones;
    FBotonEditar.Action := FacEditar;
  end;
end;

procedure TZControladorGrilla.SetBotonEliminar(const AValue: TBitBtn);
begin
  if FBotonEliminar = AValue then
    exit;
  FBotonEliminar := AValue;
  if FBotonEliminar <> nil then
  begin
    FBotonEliminar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_delete'));
    FBotonEliminar.Width := FAnchoBotones;
    FBotonEliminar.Height := FAltoBotones;
    FBotonEliminar.Action := FacEliminar;
  end;
end;

procedure TZControladorGrilla.SetBotonExportar(AValue: TBitBtn);
begin
  if FBotonExportar = AValue then
    exit;
  FBotonExportar := AValue;
  if FBotonExportar <> nil then
  begin
    FBotonExportar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_export'));
    FBotonExportar.Width := FAnchoBotones;
    FBotonExportar.Height := FAltoBotones;
    FBotonExportar.Action := FacExportar;
  end;
end;

procedure TZControladorGrilla.SetBotonSeleccionar(const AValue: TBitBtn);
begin
  if FBotonSeleccionar = AValue then
    exit;
  FBotonSeleccionar := AValue;
  if FBotonSeleccionar <> nil then
  begin
    FBotonSeleccionar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_back'));
    FBotonSeleccionar.Width := FAnchoBotones;
    FBotonSeleccionar.Height := FAltoBotones;
    FBotonSeleccionar.Action := FacSeleccionar;
  end;
end;

procedure TZControladorGrilla.SetBotonCancelar(const AValue: TBitBtn);
begin
  if FBotonCancelar = AValue then
    exit;
  FBotonCancelar := AValue;
  if FBotonCancelar <> nil then
  begin
    FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_cancel'));
    FBotonCancelar.Width := FAnchoBotones;
    FBotonCancelar.Height := FAltoBotones;
    FBotonCancelar.Action := FacCancelar;
  end;
end;

procedure TZControladorGrilla.SetBotonCerrar(const AValue: TBitBtn);
begin
  if FBotonCerrar = AValue then
    exit;
  FBotonCerrar := AValue;
  if FBotonCerrar <> nil then
  begin
    FBotonCerrar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_quit'));
    FBotonCerrar.Width := FAnchoBotones;
    FBotonCerrar.Height := FAltoBotones;
    FBotonCerrar.Action := FacCerrar;
  end;
end;

procedure TZControladorGrilla.SetBuscador(const AValue: TCustomEdit);
begin
  if FBuscador = AValue then
    exit;
  FBuscador := AValue;
  if (FBuscador <> nil) and not (csDesigning in ComponentState) then
  begin
    if Assigned(FBuscador.OnKeyDown) then
      FOldBuscadorKeyDown := FBuscador.OnKeyDown;
    FBuscador.OnKeyDown := @edBuscarKeyDown;
  end;
end;

procedure TZControladorGrilla.SetCampoIdPadre(AValue: TNumericField);
begin
  if FCampoIdPadre = AValue then
    Exit;
  FCampoIdPadre := AValue;
end;

procedure TZControladorGrilla.SetControlEdicion(AValue: TZControladorEdicion);
begin
  if FControlEdicion = AValue then
    Exit;
  FControlEdicion := AValue;
  if Assigned(FControlEdicion) then
  begin
    FControlEdicion.AltoBotones := FAltoBotones;
    FControlEdicion.AnchoBotones := FAnchoBotones;
  end;
end;

procedure TZControladorGrilla.SetDespuesBuscar(AValue: TNotifyEvent);
begin
  if FDespuesBuscar = AValue then
    Exit;
  FDespuesBuscar := AValue;
end;

procedure TZControladorGrilla.SetDespuesEliminar(AValue: TNotifyEvent);
begin
  if FDespuesEliminar = AValue then
    Exit;
  FDespuesEliminar := AValue;
end;

procedure TZControladorGrilla.SetDespuesEditar(AValue: TNotifyEvent);
begin
  if FDespuesEditar = AValue then
    Exit;
  FDespuesEditar := AValue;
end;

procedure TZControladorGrilla.SetDespuesAgregar(AValue: TNotifyEvent);
begin
  if FDespuesAgregar = AValue then
    Exit;
  FDespuesAgregar := AValue;
end;

procedure TZControladorGrilla.SetExpresionFiltro(AValue: string);
begin
  if FExpresionFiltro = AValue then
    Exit;
  FExpresionFiltro := AValue;
end;

procedure TZControladorGrilla.SetGrilla(const AValue: TCustomDBGrid);
begin
  if FGrilla = AValue then
    exit;
  FGrilla := AValue;
  if (FGrilla <> nil) and not (csDesigning in ComponentState) then
  begin
    if (FGrilla is TDBGrid) then
    begin
      //(FGrilla as TDBGrid).OnKeyDown := @GrillaKeyDown;
      //(FGrilla as TDBGrid).OnDblClick := @GrillaDblClick;
      if Assigned((FGrilla as TDBGrid).OnKeyDown) then
        FOldGrillaKeyDown := (FGrilla as TDBGrid).OnKeyDown;
      if Assigned((FGrilla as TDBGrid).OnDblClick) then
        FOldGrillaDblClick := (FGrilla as TDBGrid).OnDblClick;
      (FGrilla as TDBGrid).OnKeyDown := @GrillaKeyDown;
      (FGrilla as TDBGrid).OnDblClick := @GrillaDblClick;
    end
    else if (FGrilla is TRxDBGrid) then
    begin
      //Configuro el componente para exportar a excel
      if not Assigned(FExportGrid) then
      begin
        FExportGrid := TRxDBGridExportSpreadSheet.Create(self);
        FExportGrid.RxDBGrid := (FGrilla as TRxDBGrid);
        FExportGrid.Options := [ressExportTitle, ressExportColors,
          ressExportFooter, ressOverwriteExisting];
        FExportGrid.ShowSetupForm := False;
        FExportGrid.OpenAfterExport := True;
        FExportGrid.PageName := 'Hoja 1';
        //Creo el componente de dialogo para guardar
        if not Assigned(FFileSaveDialog) then
        begin
          FFileSaveDialog := TSaveDialog.Create(Self);
          FFileSaveDialog.DefaultExt := '.xls';
          FFileSaveDialog.Filter :=
            'Hoja de cálculo de Microsoft Excel 97-2003|*.xls|Hoja de cálculo de OpenDocument|*.ods';
        end;
      end;

      //(FGrilla as TRxDBGrid).OnKeyDown := @GrillaKeyDown;
      //(FGrilla as TRxDBGrid).OnDblClick := @GrillaDblClick;
      if Assigned((FGrilla as TRxDBGrid).OnKeyDown) then
        FOldGrillaKeyDown := (FGrilla as TRxDBGrid).OnKeyDown;
      if Assigned((FGrilla as TRxDBGrid).OnDblClick) then
        FOldGrillaDblClick := (FGrilla as TRxDBGrid).OnDblClick;
      (FGrilla as TRxDBGrid).OnKeyDown := @GrillaKeyDown;
      (FGrilla as TRxDBGrid).OnDblClick := @GrillaDblClick;
    end;
  end
  else
  begin
    if FGrilla = nil then
    begin
      if Assigned(FExportGrid) then
        FExportGrid.Free;
      if Assigned(FFileSaveDialog) then
        FFileSaveDialog.Free;
    end;
  end;
end;

procedure TZControladorGrilla.SetMostrarBotonExportar(AValue: boolean);
begin
  if FMostrarBotonExportar = AValue then
    Exit;
  FMostrarBotonExportar := AValue;
  FacExportar.Visible := AValue;
end;

procedure TZControladorGrilla.SetSeleccionarUltimo(AValue: Boolean);
begin
    if FSeleccionarUltimo=AValue then Exit;
    FSeleccionarUltimo:=AValue;
end;

procedure TZControladorGrilla.SetZDataset(const AValue: TZAbstractDataset);
begin
  if FZDataset = AValue then
    exit;
  FZDataset := AValue;
  //Si el SQL está vacío, cargo la plantilla con el formato de búsqueda
  if (FZDataset <> nil) and (FZDataset is TZQuery) and
    ((FZDataset as TZQuery).SQL.Count = 0) then
  begin
    with (FZDataset as TZQuery) do
    begin
      SQL.Add('SELECT <campo_id>, <campo_1>, ...');
      SQL.Add('FROM <tabla>');
      SQL.Add('WHERE <condicion> AND (');
      SQL.Add(':__FILTRO__ IS NULL OR');
      SQL.Add(':__FILTRO__ = ''''');
      SQL.Add('OR <campo_busqueda_1> LIKE CONCAT(''%'',:__FILTRO__,''%'')');
      SQL.Add('OR <campo_busqueda_2> LIKE CONCAT(''%'',:__FILTRO__,''%''))');
      SQL.Add('ORDER BY <orden_1>, <orden_2>');
    end;
  end;
end;

procedure TZControladorGrilla.AgregarExecute(Sender: TObject);
var
  id, idPadre: longint;
  mr: TModalResult;
begin
  if Assigned(ControlEdicion) and Assigned(ZDataset) then
  begin
    if Assigned(FAntesAgregar) then
      FAntesAgregar(Self);
    if Assigned(FCampoIdPadre) then
    begin
      idPadre := FCampoIdPadre.AsInteger;
      ControlEdicion.nuevoRegistro(idPadre);
    end
    else
      ControlEdicion.nuevoRegistro;
    mr := ControlEdicion.ShowModal;
    //Habilito acciones por si se insertó el primer registro
    HabilitarAcciones;
    //Si es un subformulario no refresco, porque todavía no se
    //guardaron los datos
    if (not ControlEdicion.EsSubform) and ((mr = mrOk) or (mr = mrYes)) then
      refrescarQuery(True);
    if Assigned(FDespuesAgregar) then
      FDespuesAgregar(Self);
  end;
end;

procedure TZControladorGrilla.BuscarExecute(Sender: TObject);
var
  filtro: string;
  CampoIndice: string;
begin
  if Assigned(FAntesBuscar) then
    FAntesBuscar(Self);
  if TipoBuscador = tbParametro then
    refrescarQuery()
  else //Establezco el filtro
  begin
    if Assigned(ZDataset) and Assigned(Buscador) and (ExpresionFiltro <> '') then
    begin
      if Buscador.Text <> '' then
      begin
        //Armo la cadena de filtro reemplazando con la búsqueda
        filtro := StringReplace(ExpresionFiltro, CAD_FILTRO, Buscador.Text,
          [rfReplaceAll, rfIgnoreCase]);
        if TipoBuscador = tbFiltro then
        begin
          ZDataset.Filter := filtro;
          ZDataset.Filtered := True;
        end
        else
        begin
          //Busco en la expresión del filtro el nombre del campo a buscar
          CampoIndice := LeftStr(ExpresionFiltro, Pos('=', ExpresionFiltro) - 1);
          if CampoIndice <> '' then
          begin
            try
              ZDataset.Locate(CampoIndice, Buscador.Text, [loCaseInsensitive]);
            finally
            end;
          end;
        end;
      end
      else
      begin
        ZDataset.Filter := '';
        ZDataset.Filtered := False;
      end;
    end;
  end;
  if Assigned(FDespuesBuscar) then
    FDespuesBuscar(Self);
end;

procedure TZControladorGrilla.CancelarExecute(Sender: TObject);
begin

end;

procedure TZControladorGrilla.EliminarExecute(Sender: TObject);
var
  id: integer;
  mr: TModalResult;
begin
  if Assigned(ControlEdicion) and Assigned(ZDataset) and (ZDataset.RecordCount > 0) then
  begin
    if Assigned(FAntesEliminar) then
      FAntesEliminar(Self);
    id := ZDataset.Fields[0].AsInteger;
    ControlEdicion.eliminarRegistro(id, AvisarBorrado);
    mr := ControlEdicion.ShowModal;
    //Habilito acciones por si se eliminaron todos los registros
    HabilitarAcciones;
    //Si es un subformulario no refresco, porque todavía no se
    //guardaron los datos
    if (not ControlEdicion.EsSubform) and ((mr = mrOk) or (mr = mrYes)) then
      refrescarQuery(True);
    if Assigned(FDespuesEliminar) then
      FDespuesEliminar(Self);
  end;
end;

procedure TZControladorGrilla.ExportarExecute(Sender: TObject);
begin
  if Assigned(FExportGrid) and Assigned(FExportGrid.RxDBGrid) and
    Assigned(FFileSaveDialog) then
  begin
    if FFileSaveDialog.Execute then
    begin
      if (not FileExistsUTF8(FFileSaveDialog.FileName)) or
        (MessageDlg('El archivo ' + FFileSaveDialog.FileName + ' ya existe. ¿Desea reemplazarlo?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        FExportGrid.FileName := FFileSaveDialog.FileName;
        FExportGrid.RxDBGrid.DataSource.DataSet.DisableControls;
        FExporting := True;
        FExportGrid.Execute;
        FExporting := False;
        FExportGrid.RxDBGrid.DataSource.DataSet.EnableControls;
      end;
    end;
  end;
end;

procedure TZControladorGrilla.EditarExecute(Sender: TObject);
var
  id: integer;
  mr: TModalResult;
begin
  if Assigned(ControlEdicion) and Assigned(ZDataset) and (ZDataset.RecordCount > 0) then
  begin
    if Assigned(FAntesEditar) then
      FAntesEditar(Self);
    id := ZDataset.Fields[0].AsInteger;
    ControlEdicion.editarRegistro(id);
    mr := ControlEdicion.ShowModal;
    //Si es un subformulario no refresco, porque todavía no se
    //guardaron los datos
    if (not ControlEdicion.EsSubform) and ((mr = mrOk) or (mr = mrYes)) then
    begin
      refrescarQuery(True);
      ZDataset.Locate(ZDataset.Fields[0].FieldName, id, []);
      if Grilla.CanFocus then
        Grilla.SetFocus;
    end;
    if Assigned(FDespuesEditar) then
      FDespuesEditar(Self);
  end;
end;

procedure TZControladorGrilla.SeleccionarExecute(Sender: TObject);
begin

end;

procedure TZControladorGrilla.GrillaDblClick(Sender: TObject);
begin
  // Si hay un evento de usuario, lo ejecuto primero
  if Assigned(FOldGrillaDblClick) then
    FOldGrillaDblClick(Sender);
  FacEditar.Execute;
end;

procedure TZControladorGrilla.QueryBeforeOpen(DataSet: TDataSet);
begin

end;

procedure TZControladorGrilla.QueryAfterOpen(DataSet: TDataSet);
begin

end;

procedure TZControladorGrilla.edBuscarKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Assigned(FOldBuscadorKeyDown) then
    FOldBuscadorKeyDown(Sender, Key, Shift);
  if Key = VK_RETURN then
  begin
    FacBuscar.Execute;
  end
  else if (Key = VK_ESCAPE) and (Buscador <> nil) then
  begin
    if Buscador.Focused then
      Buscador.Text := ''
    else
      Buscador.SelectAll;
  end
  else if Key = VK_INSERT then
  begin
    FacAgregar.Execute;
  end;
  inherited;
end;

procedure TZControladorGrilla.GrillaKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  // Si hay un evento de usuario, lo ejecuto primero
  if Assigned(FOldGrillaKeyDown) then
    FOldGrillaKeyDown(Sender, Key, Shift);
  if Key = VK_ESCAPE then
  begin
    if Buscador <> nil then
    begin
      Buscador.SetFocus;
      Buscador.SelectAll;
    end;
  end
  else if Key = VK_RETURN then
  begin
    if BotonEditar <> nil then
      EditarExecute(self)
    else if BotonSeleccionar <> nil then
      SeleccionarExecute(self);
  end
  else if Key = VK_INSERT then
  begin
    if BotonAgregar <> nil then
      AgregarExecute(self);
  end
  else if Key = VK_DELETE then
  begin
    if BotonEliminar <> nil then
      EliminarExecute(self);
  end;
end;

procedure TZControladorGrilla.SetTipoBuscador(AValue: TTipoBuscador);
begin
  if FTipoBuscador = AValue then
    Exit;
  FTipoBuscador := AValue;
end;

procedure TZControladorGrilla.refrescarQuery(enfocarBuscador: boolean);
begin
  if (ZDataset <> nil) and (ZDataset is TZQuery) then
  begin
    if ZDataset.Active then
      ZDataset.Close;

    if Buscador <> nil then
    begin
      (ZDataset as TZQuery).Params.ParamByName(CAD_FILTRO).AsString := Buscador.Text;
    end
    else
    begin
      if (ZDataset as TZQuery).Params.FindParam(CAD_FILTRO) <> nil then
      begin
        (ZDataset as TZQuery).Params.ParamByName(CAD_FILTRO).AsString := '';
      end;
    end;
    ZDataset.Prepare;
    ZDataset.Open;

    if Buscador <> nil then
    begin
      if ZDataset.RecordCount = 0 then
      begin
        if Buscador.CanFocus then
          Buscador.SetFocus;
        Buscador.SelectAll;
      end
      else if enfocarBuscador then
      begin
        if Buscador.CanFocus then
          Buscador.SetFocus;
      end
      else
      begin
        if Grilla <> nil then
        begin
          if Grilla.CanFocus then
            Grilla.SetFocus;
        end;
      end;
    end;
  end;
  if FSeleccionarUltimo then
     ZDataset.Last;
  // Habilito las acciones que correspondan;
  HabilitarAcciones;
end;

procedure TZControladorGrilla.Buscar;
begin
  refrescarQuery(True);
end;

procedure TZControladorGrilla.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FBotonBuscar <> nil) and (AComponent = BotonBuscar) then
      BotonBuscar := nil;
    if (FBotonCancelar <> nil) and (AComponent = BotonCancelar) then
      BotonCancelar := nil;
    if (FBotonCerrar <> nil) and (AComponent = BotonCerrar) then
      BotonCerrar := nil;
    if (FBotonEliminar <> nil) and (AComponent = BotonEliminar) then
      BotonEliminar := nil;
    if (FBotonEditar <> nil) and (AComponent = BotonEditar) then
      BotonEditar := nil;
    if (FBotonAgregar <> nil) and (AComponent = BotonAgregar) then
      BotonAgregar := nil;
    if (FGrilla <> nil) and (AComponent = Grilla) then
      Grilla := nil;
    if (FBuscador <> nil) and (AComponent = Buscador) then
      Buscador := nil;
    if (FZDataset <> nil) and (AComponent = ZDataset) then
      ZDataset := nil;
    if (FCampoIdPadre <> nil) and (AComponent = CampoIdPadre) then
      CampoIdPadre := nil;
    if (FControlEdicion <> nil) and (AComponent = ControlEdicion) then
      ControlEdicion := nil;
  end;
end;

procedure TZControladorGrilla.HabilitarAcciones;
var
  control, Data, acti: boolean;
begin
  control := Assigned(FControlEdicion);
  if Assigned(ZDataset) then
  begin
    Data := Assigned(ZDataset);
    acti := (ZDataset.Active);
    FacAgregar.Enabled := Assigned(FControlEdicion) and Assigned(ZDataset) and
      (ZDataset.Active);
    FacCancelar.Enabled := True;
    FacEliminar.Enabled := Assigned(FControlEdicion) and Assigned(ZDataset) and
      (ZDataset.Active) and (ZDataset.RecordCount > 0);
    FacEditar.Enabled := Assigned(FControlEdicion) and Assigned(ZDataset) and
      (ZDataset.Active) and (ZDataset.RecordCount > 0);
    FacCerrar.Enabled := True;
    FacBuscar.Enabled := Assigned(ZDataset);
    FacSeleccionar.Enabled := Assigned(ZDataset) and (ZDataset.Active) and
      (ZDataset.RecordCount > 0);
    FacExportar.Enabled := Assigned(ZDataset) and (ZDataset.Active) and
      (ZDataset.RecordCount > 0) and Assigned(FExportGrid) and Assigned(FExportGrid.RxDBGrid);
  end;
end;

constructor TZControladorGrilla.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAltoBotones := 28;
  FAnchoBotones := 88;
  FAvisarBorrado := True;
  TipoBuscador := tbParametro;
  FMostrarBotonExportar := False;
  FExporting := False;
  FSeleccionarUltimo:=False;

  //Algunas no están implementadas aún
  //*FacAgregar;
  //FacAtras;
  //*FacCancelar;
  //*FacEliminar;
  //*FacEditar;
  //FacSalir;
  //FacAceptar;
  //*FacCerrar;
  //*FacBuscar;
  //*FacSeleccionar;

  // Agregar:
  FacAgregar := TAction.Create(self);
  FacAgregar.Visible := True;
  FacAgregar.Enabled := False;
  FacAgregar.ImageIndex := 0;
  FacAgregar.Caption := '&Agregar';
  FacAgregar.OnExecute := @AgregarExecute;
  FacAgregar.ShortCut := VK_INSERT;
  FacAgregar.Hint := 'Abre una ventana para permitir el registro de nuevos datos';

  // Cancelar;
  FacCancelar := TAction.Create(self);
  FacCancelar.Visible := True;
  FacCancelar.Enabled := True;
  FacCancelar.ImageIndex := 2;
  FacCancelar.Caption := 'Ca&ncelar';
  FacCancelar.OnExecute := @CancelarExecute;
  FacCancelar.Hint := 'Cierra la ventana actual descartando las modificaciones realizadas';

  // Eliminar;
  FacEliminar := TAction.Create(self);
  FacEliminar.Visible := True;
  FacEliminar.Enabled := False;
  FacEliminar.ImageIndex := 3;
  FacEliminar.Caption := 'E&liminar';
  FacEliminar.OnExecute := @EliminarExecute;
  FacEliminar.ShortCut := VK_DELETE;
  FacEliminar.Hint :=
    'Abre una ventana para confirmar la eliminación del registro seleccionado';

  // Editar
  FacEditar := TAction.Create(self);
  FacEditar.Visible := True;
  FacEditar.Enabled := False;
  FacEditar.ImageIndex := 4;
  FacEditar.Caption := '&Editar';
  FacEditar.OnExecute := @EditarExecute;
  FacEditar.Hint := 'Abre la ventana de edición del registro seleccionado';

  // Seleccionar:
  FacSeleccionar := TAction.Create(self);
  FacSeleccionar.Visible := True;
  FacSeleccionar.Enabled := False;
  FacSeleccionar.ImageIndex := 6;
  FacSeleccionar.Caption := '&Seleccionar';
  FacSeleccionar.OnExecute := @SeleccionarExecute;

  // Buscar:
  FacBuscar := TAction.Create(self);
  FacBuscar.Visible := True;
  FacBuscar.Enabled := True;
  FacBuscar.ImageIndex := 8;
  //  FacBuscar.Caption := '&Buscar';
  FacBuscar.Caption := '';
  FacBuscar.OnExecute := @BuscarExecute;
  FacBuscar.Hint := 'Busca los registros coincidentes con los criterios especificados';

  // Cerrar;
  FacCerrar := TAction.Create(self);
  FacCerrar.Visible := True;
  FacCerrar.Enabled := True;
  FacCerrar.ImageIndex := 5;
  FacCerrar.Caption := 'Ce&rrar';
  FacCerrar.OnExecute := @CancelarExecute;
  FacCerrar.Hint := 'Cierra la ventana activa';

  // Exportar;
  FacExportar := TAction.Create(self);
  FacExportar.Visible := False;
  FacExportar.Enabled := False;
  FacExportar.ImageIndex := 3;
  FacExportar.Caption := 'E&xportar';
  FacExportar.OnExecute := @ExportarExecute;
  //FacExportar.Hint:='Exporta datos como hoja de cálculo';

  //  refrescarQuery(True);
end;

destructor TZControladorGrilla.Destroy;
begin
  inherited Destroy;
end;

initialization
{$I bt_controladorgrilla.lrs}
{$I mis_componentes.lrs}
end.
