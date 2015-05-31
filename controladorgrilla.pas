unit ControladorGrilla;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buttons, LResources, Graphics, DBGrids, StdCtrls,
  DB, sqldb, Forms, Controls, LCLType, frmedicionbase, ControladorEdicion;

const

    CAD_FILTRO = '__FILTRO__';
type

  TTipoBuscador = (
      tbParametro, //Busca registro utilizando el parámetro __FILTRO__ del query
      tbFiltro,     //Usa la propiedad "Filter" del Query (en subforms, donde no se puede cerrar y abrir el query)
      tbBusqueda   //Usa el método "Locate" del Query. No filtra, sólo ubica el registro (en subforms, donde no se puede cerrar y abrir el query)
      );

  { TControladorGrilla }

  TControladorGrilla = class(TComponent)
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
    FBotonSeleccionar: TBitBtn;
    FBotonCancelar: TBitBtn;
    FBotonCerrar: TBitBtn;
    FAltoBotones: integer;
    FAnchoBotones: integer;
    FBuscador: TCustomEdit;
    FCampoIdPadre: TNumericField;
    FControlEdicion: TControladorEdicion;
    FDespuesBuscar: TNotifyEvent;
    FDespuesEliminar: TNotifyEvent;
    FDespuesEditar: TNotifyEvent;
    FDespuesAgregar: TNotifyEvent;
    FExpresionFiltro: string;
    FGrilla: TDBGrid;
    FSQLQuery: TSQLQuery;
    FTipoBuscador: TTipoBuscador;
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
    procedure SetBotonSeleccionar(const AValue: TBitBtn);
    procedure SetBotonCancelar(const AValue: TBitBtn);
    procedure SetBotonCerrar(const AValue: TBitBtn);
    procedure SetBuscador(const AValue: TCustomEdit);
    procedure SetCampoIdPadre(AValue: TNumericField);
    procedure SetControlEdicion(AValue: TControladorEdicion);
    procedure SetDespuesBuscar(AValue: TNotifyEvent);
    procedure SetDespuesEliminar(AValue: TNotifyEvent);
    procedure SetDespuesEditar(AValue: TNotifyEvent);
    procedure SetDespuesAgregar(AValue: TNotifyEvent);
    procedure SetExpresionFiltro(AValue: string); //Tipo: Utilizar "__FILTRO__" dentro de la cadena para armar la expresión a buscar
              //Por ejemplo: "Nombre=__FILTRO__ OR Descripcion=__FILTRO__"
    procedure SetGrilla(const AValue: TDBGrid);
    procedure SetSQLQuery(const AValue: TSQLQuery);

    procedure AgregarExecute(Sender: TObject);
    procedure BuscarExecute(Sender: TObject);
    procedure CancelarExecute(Sender: TObject);
    procedure EliminarExecute(Sender: TObject);
    procedure EditarExecute(Sender: TObject);
    procedure SeleccionarExecute(Sender: TObject);
    procedure GrillaDblClick(Sender: TObject);
    procedure QueryBeforeOpen(DataSet: TDataSet);
    procedure edBuscarKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure GrillaKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SetTipoBuscador(AValue: TTipoBuscador);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure refrescarQuery(enfocarBuscador: boolean = False);
  published
    property AltoBotones: integer read FAltoBotones write SetAltoBotones default 26;
    property AnchoBotones: integer read FAnchoBotones write SetAnchoBotones default 86;
    property AntesAgregar: TNotifyEvent read FAntesAgregar write SetAntesAgregar;
    property AntesBuscar: TNotifyEvent read FAntesBuscar write SetAntesBuscar;
    property AntesEditar:TNotifyEvent read FAntesEditar write SetAntesEditar;
    property AntesEliminar: TNotifyEvent read FAntesEliminar write SetAntesEliminar;
    property AvisarBorrado: boolean
      read FAvisarBorrado write SetAvisarBorrado default True;
    property BotonAgregar: TBitBtn read FBotonAgregar write SetBotonAgregar;
    property BotonEditar: TBitBtn read FBotonEditar write SetBotonEditar;
    property BotonEliminar: TBitBtn read FBotonEliminar write SetBotonEliminar;
    property BotonSeleccionar: TBitBtn read FBotonSeleccionar write SetBotonSeleccionar;
    property BotonCancelar: TBitBtn read FBotonCancelar write SetBotonCancelar;
    property BotonCerrar: TBitBtn read FBotonCerrar write SetBotonCerrar;
    property BotonBuscar: TBitBtn read FBotonBuscar write SetBotonBuscar;
    property Buscador: TCustomEdit read FBuscador write SetBuscador;
    property CampoIdPadre: TNumericField read FCampoIdPadre write SetCampoIdPadre;
    property ControlEdicion: TControladorEdicion read FControlEdicion write SetControlEdicion;
    property DespuesAgregar: TNotifyEvent read FDespuesAgregar write SetDespuesAgregar;
    property DespuesBuscar: TNotifyEvent read FDespuesBuscar write SetDespuesBuscar;
    property DespuesEditar:TNotifyEvent read FDespuesEditar write SetDespuesEditar;
    property DespuesEliminar: TNotifyEvent read FDespuesEliminar write SetDespuesEliminar;
    property ExpresionFiltro: string read FExpresionFiltro write SetExpresionFiltro;
    property Grilla: TDBGrid read FGrilla write SetGrilla;
    property SQLQuery: TSQLQuery read FSQLQuery write SetSQLQuery;
    property TipoBuscador: TTipoBuscador read FTipoBuscador write SetTipoBuscador default tbParametro;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Mis Componentes', [TControladorGrilla]);
end;

{ TControladorGrilla }

procedure TControladorGrilla.SetBotonAgregar(const AValue: TBitBtn);
begin
  if FBotonAgregar = AValue then
    exit;
  FBotonAgregar := AValue;
  if FBotonAgregar <> nil then
  begin
    FBotonAgregar.Visible := True;
    FBotonAgregar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_add'));
    FBotonAgregar.Caption := '&Agregar';
    FBotonAgregar.Width := FAnchoBotones;
    FBotonAgregar.Height := FAltoBotones;
    FBotonAgregar.OnClick := @AgregarExecute;
  end;
end;

procedure TControladorGrilla.SetAnchoBotones(const AValue: integer);
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

procedure TControladorGrilla.SetAntesBuscar(AValue: TNotifyEvent);
begin
  if FAntesBuscar=AValue then Exit;
  FAntesBuscar:=AValue;
end;

procedure TControladorGrilla.SetAntesEliminar(AValue: TNotifyEvent);
begin
  if FAntesEliminar=AValue then Exit;
  FAntesEliminar:=AValue;
end;

procedure TControladorGrilla.SetAntesEditar(AValue: TNotifyEvent);
begin
  if FAntesEditar=AValue then Exit;
  FAntesEditar:=AValue;
end;

procedure TControladorGrilla.SetAntesAgregar(AValue: TNotifyEvent);
begin
  if FAntesAgregar=AValue then Exit;
  FAntesAgregar:=AValue;
end;

procedure TControladorGrilla.SetAvisarBorrado(AValue: boolean);
begin
  if FAvisarBorrado = AValue then
    Exit;
  FAvisarBorrado := AValue;
end;

procedure TControladorGrilla.SetBotonBuscar(const AValue: TBitBtn);
begin
  if FBotonBuscar = AValue then
    exit;
  FBotonBuscar := AValue;
  if FBotonBuscar <> nil then
  begin
    FBotonBuscar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_search'));
    FBotonBuscar.Caption := '';
    //Botón cuadrado, se asigna el ancho igual que el alto
    FBotonBuscar.Width := 25;
    FBotonBuscar.Height := 25;
    FBotonBuscar.OnClick := @BuscarExecute;
  end;
end;

procedure TControladorGrilla.SetAltoBotones(const AValue: integer);
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

procedure TControladorGrilla.SetBotonEditar(const AValue: TBitBtn);
begin
  if FBotonEditar = AValue then
    exit;
  FBotonEditar := AValue;
  if FBotonEditar <> nil then
  begin
    FBotonEditar.Visible := True;
    FBotonEditar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_edit'));
    FBotonEditar.Caption := '&Editar';
    FBotonEditar.Width := FAnchoBotones;
    FBotonEditar.Height := FAltoBotones;
    FBotonEditar.OnClick := @EditarExecute;
  end;
end;

procedure TControladorGrilla.SetBotonEliminar(const AValue: TBitBtn);
begin
  if FBotonEliminar = AValue then
    exit;
  FBotonEliminar := AValue;
  if FBotonEliminar <> nil then
  begin
    FBotonEliminar.Visible := True;
    FBotonEliminar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_delete'));
    FBotonEliminar.Caption := '&Eliminar';
    FBotonEliminar.Width := FAnchoBotones;
    FBotonEliminar.Height := FAltoBotones;
    FBotonEliminar.OnClick := @EliminarExecute;
  end;
end;

procedure TControladorGrilla.SetBotonSeleccionar(const AValue: TBitBtn);
begin
  if FBotonSeleccionar = AValue then
    exit;
  FBotonSeleccionar := AValue;
  if FBotonSeleccionar <> nil then
  begin
    FBotonSeleccionar.Visible := True;
    FBotonSeleccionar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_back'));
    FBotonSeleccionar.Caption := '&Seleccionar';
    FBotonSeleccionar.Width := FAnchoBotones;
    FBotonSeleccionar.Height := FAltoBotones;
    FBotonSeleccionar.OnClick := @SeleccionarExecute;
  end;
end;

procedure TControladorGrilla.SetBotonCancelar(const AValue: TBitBtn);
begin
  if FBotonCancelar = AValue then
    exit;
  FBotonCancelar := AValue;
  if FBotonCancelar <> nil then
  begin
    FBotonCancelar.Visible := True;
    FBotonCancelar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_cancel'));
    FBotonCancelar.Caption := '&Cancelar';
    FBotonCancelar.Width := FAnchoBotones;
    FBotonCancelar.Height := FAltoBotones;
    FBotonCancelar.OnClick := @CancelarExecute;
  end;
end;

procedure TControladorGrilla.SetBotonCerrar(const AValue: TBitBtn);
begin
  if FBotonCerrar = AValue then
    exit;
  FBotonCerrar := AValue;
  if FBotonCerrar <> nil then
  begin
    FBotonCerrar.Visible := True;
    FBotonCerrar.Glyph.Assign(CreateBitmapFromLazarusResource('btgr_quit'));
    FBotonCerrar.Caption := '&Cerrar';
    FBotonCerrar.Width := FAnchoBotones;
    FBotonCerrar.Height := FAltoBotones;
    FBotonCerrar.OnClick := @CancelarExecute;
  end;
end;

procedure TControladorGrilla.SetBuscador(const AValue: TCustomEdit);
begin
  if FBuscador = AValue then
    exit;
  FBuscador := AValue;
  if FBuscador <> nil then
  begin
    FBuscador.OnKeyDown := @edBuscarKeyDown;
  end;
end;

procedure TControladorGrilla.SetCampoIdPadre(AValue: TNumericField);
begin
  if FCampoIdPadre = AValue then
    Exit;
  FCampoIdPadre := AValue;
end;

procedure TControladorGrilla.SetControlEdicion(AValue: TControladorEdicion);
begin
  if FControlEdicion=AValue then Exit;
  FControlEdicion:=AValue;
  if Assigned(FControlEdicion) then
  begin
    FControlEdicion.AltoBotones:=FAltoBotones;
    FControlEdicion.AnchoBotones:=FAnchoBotones;
  end;
end;

procedure TControladorGrilla.SetDespuesBuscar(AValue: TNotifyEvent);
begin
  if FDespuesBuscar=AValue then Exit;
  FDespuesBuscar:=AValue;
end;

procedure TControladorGrilla.SetDespuesEliminar(AValue: TNotifyEvent);
begin
  if FDespuesEliminar=AValue then Exit;
  FDespuesEliminar:=AValue;
end;

procedure TControladorGrilla.SetDespuesEditar(AValue: TNotifyEvent);
begin
  if FDespuesEditar=AValue then Exit;
  FDespuesEditar:=AValue;
end;

procedure TControladorGrilla.SetDespuesAgregar(AValue: TNotifyEvent);
begin
  if FDespuesAgregar=AValue then Exit;
  FDespuesAgregar:=AValue;
end;

procedure TControladorGrilla.SetExpresionFiltro(AValue: string);
begin
  if FExpresionFiltro=AValue then Exit;
  FExpresionFiltro:=AValue;
end;

procedure TControladorGrilla.SetGrilla(const AValue: TDBGrid);
begin
  if FGrilla = AValue then
    exit;
  FGrilla := AValue;
  if FGrilla <> nil then
  begin
    FGrilla.OnKeyDown := @GrillaKeyDown;
    FGrilla.OnDblClick := @GrillaDblClick;
  end;
end;

procedure TControladorGrilla.SetSQLQuery(const AValue: TSQLQuery);
begin
  if FSQLQuery = AValue then
    exit;
  FSQLQuery := AValue;
  //Si el SQL está vacío, cargo la plantilla con el formato de búsqueda
  if (FSQLQuery <> nil) and (FSQLQuery.SQL.Count=0) then
  begin
    FSQLQuery.SQL.Add('-- Siempre el primer campo debe ser el ID (clave primaria)');
    FSQLQuery.SQL.Add('SELECT <campo_id>, <campo_1>, ...');
    FSQLQuery.SQL.Add('FROM <tabla>');
    FSQLQuery.SQL.Add('WHERE <condicion> AND (');
    FSQLQuery.SQL.Add(':__FILTRO__ IS NULL OR');
    FSQLQuery.SQL.Add(':__FILTRO__ = ''''');
    FSQLQuery.SQL.Add('OR <campo_busqueda_1> LIKE CONCAT(''%'',:__FILTRO__,''%''');
    FSQLQuery.SQL.Add('OR <campo_busqueda_2> LIKE CONCAT(''%'',:__FILTRO__,''%'')');
    FSQLQuery.SQL.Add('ORDER BY <orden_1>, <orden_2>');
  end;
end;

procedure TControladorGrilla.AgregarExecute(Sender: TObject);
var
  id, idPadre: longint;
  mr: TModalResult;
begin
  if Assigned(ControlEdicion) and Assigned(SQLQuery) then
  begin
    if Assigned (FAntesAgregar) then
      FAntesAgregar(Self);
    if Assigned(FCampoIdPadre) then
    begin
      idPadre := FCampoIdPadre.AsInteger;
      ControlEdicion.nuevoRegistro(idPadre);
    end
    else
      ControlEdicion.nuevoRegistro;
    mr := ControlEdicion.ShowModal;
    //Si es un subformulario no refresco, porque todavía no se
    //guardaron los datos
    if (not ControlEdicion.EsSubform) and ((mr = mrOk) or (mr = mrYes)) then
      refrescarQuery(True);
    if Assigned (FDespuesAgregar) then
      FDespuesAgregar(Self);
  end;
end;

procedure TControladorGrilla.BuscarExecute(Sender: TObject);
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
    if Assigned(SQLQuery) and Assigned(Buscador) and (ExpresionFiltro<>'') then
    begin
      if Buscador.Text<>'' then
      begin
        //Armo la cadena de filtro reemplazando con la búsqueda
        filtro:=StringReplace(ExpresionFiltro, CAD_FILTRO, Buscador.Text, [rfReplaceAll, rfIgnoreCase]);
        if TipoBuscador=tbFiltro then
        begin
          SQLQuery.Filter:=filtro;
          SQLQuery.Filtered:=true;
        end else
        begin
          //Busco en la expresión del filtro el nombre del campo a buscar
          CampoIndice:=LeftStr(ExpresionFiltro, Pos('=',ExpresionFiltro)-1);
          if CampoIndice<>'' then
          begin
            try
              SQLQuery.Locate(CampoIndice, Buscador.Text,[loCaseInsensitive]);
            finally
            end;
          end;
        end;
      end else
      begin
        SQLQuery.Filter:='';
        SQLQuery.Filtered:=false;
      end;
    end;
  end;
  if Assigned(FDespuesBuscar) then
    FDespuesBuscar(Self);
end;

procedure TControladorGrilla.CancelarExecute(Sender: TObject);
begin

end;

procedure TControladorGrilla.EliminarExecute(Sender: TObject);
var
  id: integer;
  mr: TModalResult;
begin
  if Assigned(ControlEdicion) and Assigned(SQLQuery) and (SQLQuery.RecordCount > 0) then
  begin
    if Assigned (FAntesEliminar) then
      FAntesEliminar(Self);
    id := SQLQuery.Fields[0].AsInteger;
    ControlEdicion.eliminarRegistro(id, AvisarBorrado);
    mr := ControlEdicion.ShowModal;
    //Si es un subformulario no refresco, porque todavía no se
    //guardaron los datos
    if (not ControlEdicion.EsSubform) and ((mr = mrOk) or (mr = mrYes)) then
      refrescarQuery(True);
    if Assigned (FDespuesEliminar) then
      FDespuesEliminar(Self);
  end;
end;

procedure TControladorGrilla.EditarExecute(Sender: TObject);
var
  id: integer;
  mr: TModalResult;
begin
  if Assigned(ControlEdicion) and Assigned(SQLQuery) and (SQLQuery.RecordCount > 0) then
  begin
    if Assigned (FAntesEditar) then
      FAntesEditar(Self);
    id := SQLQuery.Fields[0].AsInteger;
    ControlEdicion.editarRegistro(id);
    mr := ControlEdicion.ShowModal;
    //Si es un subformulario no refresco, porque todavía no se
    //guardaron los datos
    if (not ControlEdicion.EsSubform) and ((mr = mrOk) or (mr = mrYes)) then
    begin
      refrescarQuery(True);
      SQLQuery.Locate(SQLQuery.Fields[0].FieldName, id, []);
    end;
    if Assigned (FDespuesEditar) then
      FDespuesEditar(Self);
  end;
end;

procedure TControladorGrilla.SeleccionarExecute(Sender: TObject);
begin

end;

procedure TControladorGrilla.GrillaDblClick(Sender: TObject);
begin
  EditarExecute(self);
end;

procedure TControladorGrilla.QueryBeforeOpen(DataSet: TDataSet);
begin

end;

procedure TControladorGrilla.edBuscarKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    BuscarExecute(self);
  end
  else if (Key = VK_ESCAPE) and (Buscador <> nil) then
  begin
    if Buscador.Focused then
      Buscador.Text := ''
    else
      Buscador.SelectAll;
  end;

end;

procedure TControladorGrilla.GrillaKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
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

procedure TControladorGrilla.SetTipoBuscador(AValue: TTipoBuscador);
begin
  if FTipoBuscador=AValue then Exit;
  FTipoBuscador:=AValue;
end;

procedure TControladorGrilla.refrescarQuery(enfocarBuscador: boolean);
begin
  if SQLQuery <> nil then
  begin
    if SQLQuery.Active then
      SQLQuery.Close;

    if Buscador <> nil then
    begin
      SQLQuery.Params.ParamByName(CAD_FILTRO).AsString := Buscador.Text;
    end
    else
    begin
      if SQLQuery.Params.FindParam(CAD_FILTRO) <> nil then
      begin
        SQLQuery.Params.ParamByName(CAD_FILTRO).AsString := '';
      end;
    end;
    SQLQuery.Prepare;
    SQLQuery.Open;

    if Buscador <> nil then
    begin
      if SQLQuery.RecordCount = 0 then
      begin
        Buscador.SetFocus;
        Buscador.SelectAll;
      end
      else if enfocarBuscador then
      begin
        Buscador.SetFocus;
      end
      else
      begin
        if Grilla <> nil then
        begin
          Grilla.SetFocus;
        end;
      end;
    end;
  end;
end;

procedure TControladorGrilla.Notification(AComponent: TComponent;
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
    if (FSQLQuery <> nil) and (AComponent = SQLQuery) then
      SQLQuery := nil;
    if (FCampoIdPadre <> nil) and (AComponent = CampoIdPadre) then
      CampoIdPadre := nil;
    if (FControlEdicion <> nil) and (AComponent = ControlEdicion) then
      ControlEdicion := nil;
  end;
end;

constructor TControladorGrilla.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAltoBotones := 26;
  FAnchoBotones := 86;
  FAvisarBorrado := True;
  TipoBuscador := tbParametro;
end;

destructor TControladorGrilla.Destroy;
begin
  inherited Destroy;
end;

initialization
{$I bt_controladorgrilla.lrs}
{$I mis_componentes.lrs}
end.
