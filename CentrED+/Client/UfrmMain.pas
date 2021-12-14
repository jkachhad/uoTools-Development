(*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License, Version 1.0 only
 * (the "License").  You may not use this file except in compliance
 * with the License.
 *
 * You can obtain a copy of the license at
 * http://www.opensource.org/licenses/cddl1.php.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at
 * http://www.opensource.org/licenses/cddl1.php.  If applicable,
 * add the following below this CDDL HEADER, with the fields enclosed
 * by brackets "[]" replaced with your own identifying * information:
 *      Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 *
 *
 *      Portions Copyright 2009 Andreas Schneider
 *)
unit UfrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, OpenGLContext, GL, GLu, UGameResources, ULandscape, ExtCtrls,
  StdCtrls, Spin, UEnums, VirtualTrees, VirtualList, Buttons, math, UMulBlock,
  UWorldItem, LCLIntf, UOverlayUI, UStatics, UEnhancedMemoryStream, ActnList,
  XMLPropStorage, LazHelpHTML, fgl, ImagingClasses, dateutils, UPlatformTypes,
  UMap, UPacket, UGLFont, DOM, XMLRead, XMLWrite, strutils, ULightManager,
  UndoRedoCmd, ImagingTypes, ImagingCanvases, types, Registry,
  IniFiles, LMessages;

const
  FVLightSrcImageCount = 15; // Количество иконок виртуальных источников
  FUndoListLength = 64; // Количество элементов в списке отмены

type
  TAccessChangedListener = procedure(AAccessLevel: TAccessLevel) of object;
  TSelectionListener = procedure(AWorldItem: TWorldItem) of object;
  TScreenBufferState = (sbsValid, sbsIndexed, sbsFiltered);
  TScreenBufferStates = set of TScreenBufferState;

  TBlockInfoList = specialize TFPGList<PBlockInfo>;

  TGhostTile = class(TStaticItem)
  public
    CenterX, CenterY: Word; // Точки привязки к курсору (центральный тайл для объектов размером больше 1 тайла)
  end;
  TPacketList = specialize TFPGObjectList<TPacket>;
  PPacketList = ^TPacketList;
  TAccessChangedListeners = specialize TFPGList<TAccessChangedListener>;
  TSelectionListeners = specialize TFPGList<TSelectionListener>;

  TTileHintInfo = record
    Column: Byte;
    Obj: String;
    Name: String;
    Flags: String;
    ObjRect: TRect;
    NameRect: TRect;
    FlagsRect: TRect;
  end;

  TGroupTile = record
    ID: LongWord;
  end;

  TEntryTile = record
    ID: LongWord;
    Hue: Word;
    X,Y: Smallint;
    Z: ShortInt;
  end;

  TBrushTile = record
    ID: Word;
    Chance: Float;
    Mask: Byte;
    Brush1: ^TGroupBrush;
    Brush2: ^TGroupBrush;
  end;
  PBrushTile = ^TBrushTile;

  TGroupEntry = record
    ID: Word;
    Name: string;
    Image: TSingleImage;
    Count: Word;     // Число тайлов в объекте
    ETile: ^TEntryTile;
  end;
  PGroupEntry = ^TGroupEntry;

  TGroupBrushEdge = record
    ID: Word;
    CountDR: Word;
    BTileDR: ^PBrushTile;
    CountDL: Word;
    BTileDL: ^PBrushTile;
    CountUR: Word;
    BTileUR: ^PBrushTile;
    CountUL: Word;
    BTileUL: ^PBrushTile;
    CountLL: Word;
    BTileLL: ^PBrushTile;
    CountUU: Word;
    BTileUU: ^PBrushTile;
  end;
  PGroupBrushEdge = ^TGroupBrushEdge;

  TGroupBrush = record
    ID: Word;
    Name: string;
    Image: TSingleImage;
    Count: Word;     // Число тайлов в объекте
    BTile: ^PBrushTile;

    ECount: Word;    // Число тайлов перехода (в <Edge />)
    EdgeId: ^PWord;  // �ндификаторы кистей переходов (ссылка на кисть в <Edge />)
    BEdges: ^PGroupBrushEdge; // Тайлы переходов
  end;
  PGroupBrush = ^TGroupBrush;

  TEntryList = record
    Entry: ^PGroupEntry;
    Count: Word;
  end;

  TBrushList = record
    Brush: ^PGroupBrush;
    Count: Word;
    Tiles: array[0..$3FFF] of TBrushTile;
  end;

  TGroupNode = record
    Name : string;
    Color: TColor;
    Bold : Boolean;
    Ital : Boolean;
    Items: LongWord; // число элементов (тайлы, объекты, кисти и тд) в группе (включая подгруппы)
    Nodes: Word;
    ID   : LongWord;
    Links: Word;             // число элементов в GLink
    lids : ^LongWord;
    GLink: ^PVirtualNode;    // сылки на группы
    Count: LongWord;         // число элементов в GTile
    GTile: ^TGroupTile;
    Entries: LongWord;       // число элементов в Entry
    Entry: ^PGroupEntry;     // сылки на объекты из TilesEntry.xml
    Brushes: LongWord;       // число элементов в Brush
    Brush: ^PGroupBrush;     // сылки на объекты из TilesBrush.xml
  end;
  PGroupNode = ^TGroupNode;

  // SurfaceInf.xml types start
  TSurfTile = record
    Tile: ^LongWord; // ID тайлов лендов и итемов
    Hash: ^LongWord; // Surface's тэги для них
    Count: Word;
  end;
  PSurfTile = ^TSurfTile;

  TSurfGrad = record
    Grad: ^TSurfTile; // списки тайлов в Surface'ах, рассортированные по Category+Type
    Hash: ^LongWord;  // SurfaceCategory + ItemType
    Count: Word;
  end;
  PSurfGrad = ^TSurfGrad;

  TSurfInfo = record
    Name  : string;
    TileID: LongWord;
    TileHash:  LongWord; // Кеш Имени (Name)
    GradHash: ^LongWord; // Кэши типов тайлов в объекте
    GradCount: Word;     // Чмсло GradHash
  end;
  PSurfInfo = ^TSurfInfo;

  TSurfGroup = record
    Name : string;       // Имя категории
    Info : ^TSurfInfo;   // Список поверхностей в категории
    Count: Word;         // Длинна списка поверхностей
  end;
  PSurfGroup = ^TSurfGroup;

  TSurfsList = record
    Group: ^TSurfGroup;
    GroupCount:  Word;

    Grads: ^TSurfGrad;   // Контейнер для хранения памяти
    GradsCount:  Word;

    Tiles: ^PSurfGrad;
    TilesCount:  Word;
  end;
  // SurfaceInf.xml types end

  TLightTile = record
    image: Byte;
    color: Byte;
  end;
  PLightTile = ^TLightTile;

  { TfrmMain }

  TfrmMain = class(TForm)
    acSelect: TAction;
    acDraw: TAction;
    acMove: TAction;
    acElevate: TAction;
    acDelete: TAction;
    acHue: TAction;
    acBoundaries: TAction;
    acFilter: TAction;
    acFlat: TAction;
    acNoDraw: TAction;
    acLightlevel: TAction;
    acTerrain: TAction;
    acStatics: TAction;
    acSelection: TAction;
    acSurfElevate: TAction;
    acSurfStretch: TAction;
    acSurfSmooth: TAction;
    acFill: TAction;
    acRedo: TAction;
    acWalkable: TAction;
    acUndo: TAction;
    acVirtualLayer: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    btnAddLocation: TSpeedButton;
    btnAddRandom: TSpeedButton;
    btnClearLocations: TSpeedButton;
    btnClearRandom: TSpeedButton;
    btnDeleteLocation: TSpeedButton;
    btnDeleteRandom: TSpeedButton;
    btnGoTo: TButton;
    btnRandomPresetDelete: TSpeedButton;
    btnRandomPresetSave: TSpeedButton;
    cbRandomPreset: TComboBox;
    cbStatics: TCheckBox;
    cbTerrain: TCheckBox;
    edChat: TEdit;
    edFilter: TEdit;
    edSearchID: TEdit;
    edX: TSpinEdit;
    edY: TSpinEdit;
    gbRandom: TGroupBox;
    gbGoTo: TGroupBox;
    ImageList1: TImageList;
    lblTileInfoOLabel: TLabel;
    lblTileInfoIDLabel: TLabel;
    lblTileInfoHLabel: TLabel;
    lblTileInfoIDValue: TLabel;
    lblTileInfoCLabel: TLabel;
    lblTileInfoWLabel: TLabel;
    lblTileInfoYLabel: TLabel;
    lblTileInfoXValue: TLabel;
    lblTileInfoXLabel: TLabel;
    lblTileInfoZValue: TLabel;
    lblTileInfoHueValue: TLabel;
    lblTileInfoHueLabel: TLabel;
    lblChatHeaderCaption: TLabel;
    lblFilter: TLabel;
    lblTileInfoZLabel: TLabel;
    lblTileInfoYValue: TLabel;
    lblTileInfoHValue: TLabel;
    lblTileInfoWValue: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuMiscTileListLarge: TMenuItem;
    mnuSeparator9: TMenuItem;
    mnuSeparator8: TMenuItem;
    mnuTileListDrawInfo: TMenuItem;
    mnuMiscTileListTable: TMenuItem;
    mnuMiscTileListSmall: TMenuItem;
    mnuSeparator10: TMenuItem;
    mnuMiscTileListCentre: TMenuItem;
    mnuMiscTileListDrawInfo: TMenuItem;
    mnuMiscTileListMidle: TMenuItem;
    mnuMiscTileListClip: TMenuItem;
    mnuMiscTileListStretch: TMenuItem;
    mnuMiscTileList: TMenuItem;
    mnuEng2Com: TMenuItem;
    mnuTileListMidle: TMenuItem;
    mnuTileListTable: TMenuItem;
    mnuTileListSmall: TMenuItem;
    mnuTileListLarge: TMenuItem;
    mnuTileListStretch: TMenuItem;
    mnuTileListClip: TMenuItem;
    mnuTileListCentre: TMenuItem;
    mnuSeparator7: TMenuItem;
    mnuTileList: TMenuItem;
    mnuSeparator6: TMenuItem;
    mnuSetLanguage: TMenuItem;
    mnuAutoShowFilterWindow: TMenuItem;
    mnuSelection: TMenuItem;
    mnuSurfElevate: TMenuItem;
    mnuSurfStretch: TMenuItem;
    mnuSurfSmooth: TMenuItem;
    mnuFill: TMenuItem;
    mnuShowNoDrawTiles: TMenuItem;
    mnuEngCom: TMenuItem;
    mnuRusCom: TMenuItem;
    mnuSupport: TMenuItem;
    mnuSeparator5: TMenuItem;
    mnuDocs: TMenuItem;
    mnuShowBlocks: TMenuItem;
    mnuGrabBoundaries: TMenuItem;
    mnuGrabBoundMinZ: TMenuItem;
    mnuGrabBoundMaxZ: TMenuItem;
    mnuGrabBoundMinX: TMenuItem;
    mnuGrabBoundMaxX: TMenuItem;
    mnuGrabBoundMinY: TMenuItem;
    mnuGrabBoundMaxY: TMenuItem;
    mnuZoom300: TMenuItem;
    mnuZoom400: TMenuItem;
    mnuZoom033: TMenuItem;
    mnuZoom025: TMenuItem;
    mnuZoom150: TMenuItem;
    mnuZoom200: TMenuItem;
    mnuZoom075: TMenuItem;
    mnuZoom050: TMenuItem;
    mnuZoom100: TMenuItem;
    mnuShowBridges: TMenuItem;
    mnuWindowedMode: TMenuItem;
    mnuMakeScreenShot: TMenuItem;
    mnuShowWater: TMenuItem;
    mnuShowSurfaces: TMenuItem;
    mnuShowRoofs: TMenuItem;
    mnuShowFoliage: TMenuItem;
    mnuAutoHideRandomList: TMenuItem;
    mnuAutoHideGroupList: TMenuItem;
    mnuSeparator4: TMenuItem;
    mnuReloadGroups: TMenuItem;
    mnuGrabFilterTileID: TMenuItem;
    mnuGrabFilterHue: TMenuItem;
    mnuGrabVirtualLayerZ: TMenuItem;
    mnuShowGrid: TMenuItem;
    mnuShowWalls: TMenuItem;
    mnuShowLightSource: TMenuItem;
    mnuWhiteBackground: TMenuItem;
    mnuSecurityQuestion: TMenuItem;
    mnuShowAnimations: TMenuItem;
    mnuSettings: TMenuItem;
    mnuFlatShowHeight: TMenuItem;
    mnuGrabHue: TMenuItem;
    mnuGrabTileID: TMenuItem;
    mnuRegionControl: TMenuItem;
    mnuVirtualLayer: TMenuItem;
    mnuLargeScaleCommands: TMenuItem;
    mnuSetHue: TMenuItem;
    mnuGoToClient: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuSeparator3: TMenuItem;
    mnuBoundaries: TMenuItem;
    mnuSelect: TMenuItem;
    mnuDraw: TMenuItem;
    mnuMove: TMenuItem;
    mnuElevate: TMenuItem;
    mnuDelete: TMenuItem;
    mnuAddToRandom: TMenuItem;
    mnuFlush: TMenuItem;
    mnuShutdown: TMenuItem;
    mnuSeparator2: TMenuItem;
    mnuAccountControl: TMenuItem;
    mnuAdministration: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuExit: TMenuItem;
    mnuDisconnect: TMenuItem;
    mnuCentrED: TMenuItem;
    oglGameWindow: TOpenGLControl;
    pbRadar: TPaintBox;
    pcLeft: TPageControl;
    pmZoomSettings: TPopupMenu;
    pmViewTerrainSettings: TPopupMenu;
    pmGrabTileInfo: TPopupMenu;
    pmNoDrawSettings: TPopupMenu;
    pmViewStaticSettings: TPopupMenu;
    pmTileList: TPopupMenu;
    pmTools: TPopupMenu;
    pmClients: TPopupMenu;
    pnlChat: TPanel;
    pnlChatHeader: TPanel;
    pmFlatViewSettings: TPopupMenu;
    spChat: TSplitter;
    spGroupList1: TSplitter;
    spTileList: TSplitter;
    spGroupList: TSplitter;
    tbFill: TToolButton;
    tbSurfSmooth: TToolButton;
    tbSurfStretch: TToolButton;
    tbSurfElevate: TToolButton;
    tbSelection: TToolButton;
    tbFilter: TToolButton;
    tbFlat: TToolButton;
    tbSeparator6: TToolButton;
    tbSeparator7: TToolButton;
    tbSeparator8: TToolButton;
    tbSeparator9: TToolButton;
    tbRedo: TToolButton;
    tbZoom: TToolButton;
    tbNoDraw: TToolButton;
    tbSeparator2: TToolButton;
    tbUndo: TToolButton;
    tbLightlevel: TToolButton;
    tbWalkable: TToolButton;
    tmSelectNode: TTimer;
    tmSettingsClose: TTimer;
    tsNavigation: TTabSheet;
    tbSetHue: TToolButton;
    tmGrabTileInfo: TTimer;
    tmMovement: TTimer;
    tbSeparator5: TToolButton;
    tbRadarMap: TToolButton;
    tbVirtualLayer: TToolButton;
    tsObjects: TTabSheet;
    tbMain: TToolBar;
    tbDisconnect: TToolButton;
    tbSeparator1: TToolButton;
    tbSelect: TToolButton;
    tbDrawTile: TToolButton;
    tbMoveTile: TToolButton;
    tbElevateTile: TToolButton;
    tbDeleteTile: TToolButton;
    tbSeparator3: TToolButton;
    tbBoundaries: TToolButton;
    tbSeparator4: TToolButton;
    tbTerrain: TToolButton;
    tbStatics: TToolButton;
    tsTiles: TTabSheet;
    tvGroups: TVirtualStringTree;
    vdtTiles: TVirtualList;
    vdtRandom: TVirtualDrawTree;
    vdlRandom: TVirtualList;
    vstChat: TVirtualStringTree;
    vstLocations: TVirtualStringTree;
    vstClients: TVirtualStringTree;
    XMLPropStorage1: TXMLPropStorage;
    procedure acBoundariesExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDrawExecute(Sender: TObject);
    procedure acElevateExecute(Sender: TObject);
    procedure acFillExecute(Sender: TObject);
    procedure acFilterExecute(Sender: TObject);
    procedure acFlatExecute(Sender: TObject);
    procedure acHueExecute(Sender: TObject);
    procedure acLightlevelExecute(Sender: TObject);
    procedure acMoveExecute(Sender: TObject);
    procedure acNoDrawExecute(Sender: TObject);
    procedure acSelectExecute(Sender: TObject);
    procedure acSelectionExecute(Sender: TObject);
    procedure acStaticsExecute(Sender: TObject);
    procedure acSurfElevateExecute(Sender: TObject);
    procedure acSurfSmoothExecute(Sender: TObject);
    procedure acSurfStretchExecute(Sender: TObject);
    procedure acTerrainExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acVirtualLayerExecute(Sender: TObject);
    procedure acWalkableExecute(Sender: TObject);
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure btnAddLocationClick(Sender: TObject);
    procedure btnAddRandomClick(Sender: TObject);
    procedure btnClearLocationsClick(Sender: TObject);
    procedure btnClearRandomClick(Sender: TObject);
    procedure btnDeleteLocationClick(Sender: TObject);
    procedure btnDeleteRandomClick(Sender: TObject);
    procedure btnGoToClick(Sender: TObject);
    procedure btnRandomPresetDeleteClick(Sender: TObject);
    procedure btnRandomPresetSaveClick(Sender: TObject);
    procedure cbRandomPresetChange(Sender: TObject);
    procedure cbStaticsChange(Sender: TObject);
    procedure cbTerrainChange(Sender: TObject);
    procedure edChatKeyPress(Sender: TObject; var Key: char);
    procedure edFilterEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edSearchIDExit(Sender: TObject);
    procedure edSearchIDKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure lblChatHeaderCaptionClick(Sender: TObject);
    procedure lblChatHeaderCaptionMouseEnter(Sender: TObject);
    procedure lblChatHeaderCaptionMouseLeave(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuAccountControlClick(Sender: TObject);
    procedure mnuAutoHideGroupListClick(Sender: TObject);
    procedure mnuAutoHideRandomListClick(Sender: TObject);
    procedure mnuDisconnectClick(Sender: TObject);
    procedure mnuDocsClick(Sender: TObject);
    procedure mnuEng2ComClick(Sender: TObject);
    procedure mnuEngComClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuFlatShowHeightClick(Sender: TObject);
    procedure mnuFlushClick(Sender: TObject);
    procedure mnuGoToClientClick(Sender: TObject);
    procedure GrabBoundaries(Sender: TObject);
    procedure mnuGrabFilterHueClick(Sender: TObject);
    procedure mnuGrabFilterTileIDClick(Sender: TObject);
    procedure mnuGrabHueClick(Sender: TObject);
    procedure mnuGrabTileIDClick(Sender: TObject);
    procedure mnuGrabVirtualLayerZClick(Sender: TObject);
    procedure mnuLargeScaleCommandsClick(Sender: TObject);
    procedure mnuMakeScreenShotClick(Sender: TObject);
    procedure mnuRegionControlClick(Sender: TObject);
    procedure mnuReloadGroupsClick(Sender: TObject);
    procedure mnuRusComClick(Sender: TObject);
    procedure mnuSetLanguageClick(Sender: TObject);
    procedure mnuShowAnimationsClick(Sender: TObject);
    procedure mnuShowBlocksClick(Sender: TObject);
    procedure mnuShowGridClick(Sender: TObject);
    procedure mnuShowLightSourceClick(Sender: TObject);
    procedure mnuShowNoDrawTilesClick(Sender: TObject);
    procedure mnuShowStaticsOptionClick(Sender: TObject);
    procedure mnuShutdownClick(Sender: TObject);
    procedure mnuTileListDrawClick(Sender: TObject);
    procedure mnuTileListViewClick(Sender: TObject);
    procedure mnuWhiteBackgroundClick(Sender: TObject);
    procedure mnuWindowedModeClick(Sender: TObject);
    procedure mnuZoomClick(Sender: TObject);
    procedure oglGameWindowDblClick(Sender: TObject);
    procedure oglGameWindowKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure oglGameWindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure oglGameWindowMouseEnter(Sender: TObject);
    procedure oglGameWindowMouseLeave(Sender: TObject);
    procedure oglGameWindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure oglGameWindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure oglGameWindowMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure oglGameWindowPaint(Sender: TObject);
    procedure oglGameWindowResize(Sender: TObject);
    procedure pbRadarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbRadarPaint(Sender: TObject);
    procedure pcLeftResize(Sender: TObject);
    procedure pmGrabTileInfoPopup(Sender: TObject);
    procedure DropedownMenusClose(Sender: TObject);
    procedure spGroupListMoved(Sender: TObject);
    procedure spTileListMoved(Sender: TObject);
    procedure tbFilterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbFilterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbRadarMapClick(Sender: TObject);
    procedure tmGrabTileInfoTimer(Sender: TObject);
    procedure tmMovementTimer(Sender: TObject);
    procedure tmSelectNodeTimer(Sender: TObject);
    procedure tmSettingsCloseTimer(Sender: TObject);
    procedure tvGroupFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tvSelectGroupsChanged(Sender: TObject);
    procedure vdtRandomClick(Sender: TObject);
    procedure vdtRandomDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vdtRandomDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure vdtRandomLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure vdtRandomSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Stream: TStream);
    procedure vdtRandomUpdating(Sender: TBaseVirtualTree; State: TVTUpdateState);
    procedure vdtTilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vdtTilesClick(Sender: TObject);
    procedure vdtTilesDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vdtTilesDrawHint(Sender: TBaseVirtualTree; HintCanvas: TCanvas;
      Node: PVirtualNode; const R: TRect; Column: TColumnIndex);
    procedure vdtTilesDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vdtTilesEnter(Sender: TObject);
    procedure vdtTilesGetHintSize(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var R: TRect);
    procedure vdtTilesKeyPress(Sender: TObject; var Key: char);
    procedure vdtTilesScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vstChatClick(Sender: TObject);
    procedure vstChatFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstChatGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstChatPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstClientsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstClientsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstLocationsDblClick(Sender: TObject);
    procedure vstLocationsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode
      );
    procedure vstLocationsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstLocationsLoadNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure vstLocationsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
    procedure vstLocationsSaveNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure XMLPropStorage1RestoreProperties(Sender: TObject);
    procedure XMLPropStorage1SavingProperties(Sender: TObject);
  protected
    { Members }
    FAppDir: String;      // .\
    FLocalDir: String;    // .\..\LocalData\
    FConfigDir: String;   // {$User}\AppData\Local\CentrED-plus\                      - зависит от реестра
    FProfileDir: String;  // {$User}\AppData\Local\CentrED-plus\Profiles\{$Profile}\  - зависит от реестра
    FX: Integer;
    FY: Integer;
    FDrawDistance: Integer;
    FLowOffsetX: Integer;
    FLowOffsetY: Integer;
    FHighOffsetX: Integer;
    FHighOffsetY: Integer;
    FRangeX: Integer;
    FRangeY: Integer;
    FLandscape: TLandscape;
    FTextureManager: TLandTextureManager;
    FScreenBuffer: TScreenBuffer;
    FScreenBufferState: TScreenBufferStates;
    FCurrentTile: TWorldItem;
    FSelectedTile: TWorldItem;
    FVirtualTiles: TWorldItemList;
    FVLayerImage: TSingleImage;
    FVLayerMaterial: TMaterial;
    FVLightSrcImage: array[1..FVLightSrcImageCount] of TSingleImage;
    FVLightSrcMaterial: ^TMaterial;
    FOverlayUI: TOverlayUI;
    FLocationsFile: string;
    FRandomPresetsFile: string;
    FRandomPresetsDoc: TXMLDocument;
    FLastDraw: TDateTime;
    FAccessChangedListeners: TAccessChangedListeners;
    FRepaintNeeded: Boolean;
    FSelection: TRect;
    FUndoList: PPacketList;
    FUndoListArray: array[1..FUndoListLength] of TPacketList;
    FUndoListFirstIndex: Word;
    FundoListLastIndex: Word;
    FEntryList: TEntryList;
    FBrushList: TBrushList;
    FSurfsList: TSurfsList;

    FGroupsSelectionUndoRedoCommandGroup: TUndoRedoCommandGroup;
    FGroupsSelectionUndoRedoManager: TUndoRedoManager;
    FTilesSelectionUndoRedoCommandGroup: TUndoRedoCommandGroup;
    FTilesSelectionUndoRedoManager: TUndoRedoManager;

    FGLFont: TGLFont;
    FSelectionListeners: TSelectionListeners;
    FTileHint: TTileHintInfo;
    FLightManager: TLightManager;
    FVisibleTiles: TBits;
    FLightSourceTiles: PLightTile;
    { Methods }
    function  GetNextUndoList: PPacketList;
    function  LoadListError(condition: Boolean; filename, message : string): Boolean;
    procedure LoadVisibleTiles(AFileName: String);
    procedure LoadLightSourceTiles(AFileName: String);
    procedure LoadEntryTilesList;
    procedure LoadBrushTilesList;
    procedure LoadSurfsTilesList;
    procedure BuildGroupList;
    procedure BuildTileList;
    procedure FreeGroupLists;
    function  ConfirmAction: Boolean;
    function  FindRandomPreset(AName: String): TDOMElement;
    procedure ForceUpdateCurrentTile;
    procedure GetDrawOffset(AX, AY: Integer; out DrawX, DrawY: Integer); inline;
    function  GetInternalTileID(ATile: TWorldItem): LongWord;
    function  GetSelectedRect: TRect;
    procedure InitRender;
    procedure InitSize;
    procedure LoadLocations;
    procedure LoadRandomPresets;
    procedure MoveBy(AOffsetX, AOffsetY: Integer); inline;
    procedure PrepareMapCell(AMapCell: TMapCell);
    procedure PrepareScreenBlock(ABlockInfo: PBlockInfo);
    procedure ProcessToolState;
    procedure ProcessAccessLevel;
    procedure RebuildScreenBuffer;
    procedure Render;
    procedure SaveLocations;
    procedure SaveRandomPresets;
    procedure SetCurrentTile(const AValue: TWorldItem);
    procedure SetDarkLights; inline;
    procedure SetNormalLights; inline;
    procedure SetSelectedTile(const AValue: TWorldItem);
    procedure SetX(const AValue: Integer);
    procedure SetY(const AValue: Integer);
    procedure UpdateCurrentTile;
    procedure UpdateCurrentTile(AX, AY: Integer);
    procedure UpdateFilter;
    procedure UpdateSelection;
    procedure WriteChatMessage(ASender, AMessage: string);
    { Events }
    procedure OnClientHandlingPacket(ABuffer: TEnhancedMemoryStream);
    procedure OnLandscapeChanged;
    procedure OnMapChanged(AMapCell: TMapCell);
    procedure OnNewBlock(ABlock: TBlock);
    procedure OnStaticDeleted(AStaticItem: TStaticItem);
    procedure OnStaticElevated(AStaticItem: TStaticItem);
    procedure OnStaticHued(AStaticItem: TStaticItem);
    procedure OnStaticInserted(AStaticItem: TStaticItem);
    procedure OnTileRemoved(ATile: TMulBlock);
  public
    { Fields }
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
    property Landscape: TLandscape read FLandscape;
    property CurrentTile: TWorldItem read FCurrentTile write SetCurrentTile;
    property SelectedTile: TWorldItem read FSelectedTile write SetSelectedTile;
    property LightManager: TLightManager read FLightManager;
    property AppDir: string read FAppDir;
    property LocalDir: string read FLocalDir;
    property ConfigDir: string read FConfigDir;
    property ProfileDir: string read FProfileDir;

    { Methods }
    procedure InvalidateFilter;
    procedure InvalidateScreenBuffer;
    procedure RegisterAccessChangedListener(AListener: TAccessChangedListener);
    procedure RegisterSelectionListener(AListener: TSelectionListener);
    procedure SetPos(AX, AY: Word);
    procedure SwitchToSelection;
    procedure UnregisterAccessChangedListener(AListener: TAccessChangedListener);
    procedure UnregisterSelectionListener(AListener: TSelectionListener);
  public
    {Localization}
    lbFormTitleAccount: string;
    lbFormTitleProfile: string;
    lbBottomCursorVLayer1: string;
    lbBottomCursorVLayer2: string;
    lbBottomCursorItemID: string;
    lbBottomCursorLandID: string;
    lbBottomCursorPosX: string;
    lbBottomCursorPosY: string;
    lbBottomCursorPosZ: string;
    lbBottomCursorItemHue: string;
    lbToolbarUndo: string;

    lbDlgWindowedModeSwitchCaption: string;
    lbDlgWindowedModeSwitch: string;
    lbScreenShotMsg: string;
    lbUserLoginedMsg: string;
    lbUserLogoutedMsg: string;
    lbDlgGetDcErrCaption: string;
    lbDlgGetDcErr: string;
    lbDlgFreeDcErrCaption: string;
    lbDlgFreeDcErr: string;
    lbDlgCnangedAccessCaption: string;
    lbDlgCnangedAccess: string;
    lbDlgBlockedAccessCaption: string;
    lbDlgBlockedAccess: string;

    lbDlgSaveRandPrsCaption: string;
    lbDlgSaveRandPrs: string;
    lbDlgSearchIdErrCaption: string;
    lbDlgSearchIdErr: string;
    lbDlgNotFoundErrCaption: string;
    lbDlgNotFoundErr: string;

    lbDlgDelConfCaption: string;
    lbDlgDelConf: string;
    lbDlgNewQuerryCaption: string;
    lbDlgNewQuerry: string;

  end; 

var
  frmMain: TfrmMain;

implementation

uses
  UdmNetwork, UArt, UTexture, UHue, UTiledata, UAdminHandling, UPackets,
  UfrmAccountControl, UGraphicHelper, ImagingComponents, UfrmDrawSettings,
  UfrmBoundaries, UfrmElevateSettings, UfrmConfirmation, UfrmMoveSettings,
  UfrmAbout, UPacketHandlers, UfrmHueSettings, UfrmRadar, UfrmLargeScaleCommand,
  UfrmLogin, UResourceManager, UfrmVirtualLayer, UfrmFilter, UfrmRegionControl,
  Logging, LConvEncoding, LCLType, UfrmLightlevel, vinfo, Imaging, Language,
  UfrmEditAccount, UfrmFillSettings, UfrmSelectionSettings, UfrmInitialize,
  UfrmSurfElevateSettings, UfrmSurfStretchSettings, UfrmSurfSmoothSettings, Crc32Hash;

{$I version.inc}

type
  TGLArrayf4 = array[0..3] of GLfloat;
  PTileInfo = ^TTileInfo;
  TTileInfo = record
    ID: LongWord;
    ptr: Pointer;
  end;
  PChatInfo = ^TChatInfo;
  TChatInfo = record
    Time: TDateTime;
    Sender: string;
    Msg: string;
  end;
  PLocationInfo = ^TLocationInfo;
  TLocationInfo = record
    X: Word;
    Y: Word;
    Name: string;
  end;
  PClientInfo = ^TClientInfo;
  TClientInfo = record
    Name: string;
    AccessLevel: TAccessLevel;
    LogonDateTime : TDateTime;
    //Time: string;
    //Map: Byte;
    //X: Word;
    //Y: Word;
  end;

const
  CScreenBufferValid = [sbsValid, sbsIndexed, sbsFiltered];

function IsInRect(const AX, AY: Integer; const ARect: TRect): Boolean; inline;
begin
  Result := (AX >= ARect.Left) and
            (AX <= ARect.Right) and
            (AY >= ARect.Top) and
            (AY <= ARect.Bottom);
end;

{ TfrmMain }

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuFlatShowHeightClick(Sender: TObject);
begin
  tbFlat.Down := acFlat.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.mnuShowNoDrawTilesClick(Sender: TObject);
begin
  tbNoDraw.Down := acNoDraw.Checked;
  RebuildScreenBuffer;
  FRepaintNeeded := True;
end;

procedure TfrmMain.mnuShowLightSourceClick(Sender: TObject);
begin
  tbNoDraw.Down := acNoDraw.Checked;
  RebuildScreenBuffer;
  FRepaintNeeded := True;
end;

procedure TfrmMain.mnuShowGridClick(Sender: TObject);
begin
  tbTerrain.Down := acTerrain.Checked;
  mnuShowBlocks.Checked := False;
  RebuildScreenBuffer;
  FRepaintNeeded := True;
end;

procedure TfrmMain.mnuShowBlocksClick(Sender: TObject);
begin
  tbTerrain.Down := acTerrain.Checked;
  mnuShowGrid.Checked := False;
  RebuildScreenBuffer;
  FRepaintNeeded := True;
end;

procedure TfrmMain.mnuShowStaticsOptionClick(Sender: TObject);
begin
  tbStatics.Down := acStatics.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.mnuFlushClick(Sender: TObject);
begin
  dmNetwork.Send(TFlushServerPacket.Create);
end;

procedure TfrmMain.mnuGoToClientClick(Sender: TObject);
var
  mpos: TPoint;
  node: PVirtualNode;
  clientInfo: PClientInfo;
begin
  mpos := vstClients.ScreenToClient(Mouse.CursorPos);
  node := vstClients.GetNodeAt(mpos.X, mpos.Y);// .GetFirstSelected;
  if node <> nil then
  begin
    clientInfo := vstClients.GetNodeData(node);
    dmNetwork.Send(TGotoClientPosPacket.Create(clientInfo^.Name));
  end;

end;

procedure TfrmMain.mnuGrabTileIDClick(Sender: TObject);
var
  internalTileID: LongWord;
  item: PVirtualItem;
  tileInfo: PTileInfo;
  treeNode: PVirtualNode;

  function TileInNode(Node: PVirtualNode; TileID: LongWord) : Boolean;
  var
    nodeData: ^TGroupNode;
    i: Integer;
  begin
    Result := False;
    nodeData := tvGroups.GetNodeData(Node);
    for i := 0 to nodeData^.Count - 1 do
    begin
      if nodeData^.GTile[i].ID = TileID then
      begin
        Result := True;
        break;
      end;
    end;
  end;

begin
  Logger.Send([lcClient, lcDebug], 'TfrmMain.mnuGrabTileIDClick', TRUE);
  if CurrentTile <> nil then
  begin
    internalTileID := GetInternalTileID(CurrentTile);

    // Выбираем группы
    if (not cbStatics.Checked) and (not cbTerrain.Checked) then
    begin
      treeNode := tvGroups.GetFirst();
      while treeNode <> nil do
      begin
        if TileInNode(treeNode, internalTileID) then
        begin
          tvGroups.Selected[treeNode] := True;
          tvGroups.FocusedNode := treeNode;
          if toMultiSelect in tvGroups.TreeOptions.SelectionOptions
            then break;
        end;
          treeNode := tvGroups.GetNext(treeNode);
      end;
    end;

    Logger.Send([lcClient, lcDebug], 'TfrmMain.mnuGrabTileIDClick', internalTileID);
    // Выбираем тайл
    item := vdtTiles.GetFirst;
    while item <> nil do
    begin
      tileInfo := vdtTiles.GetNodeData(item);
      if tileInfo^.ID = internalTileID then
      begin
        vdtTiles.ClearSelection;
        vdtTiles.Selected[item] := True;
        vdtTiles.FocusedNode := item;
        Break;
      end;
      item := vdtTiles.GetNext(item);
    end;
  end;
  Logger.Send([lcClient, lcDebug], 'TfrmMain.mnuGrabTileIDClick', FALSE);
end;

procedure TfrmMain.mnuGrabHueClick(Sender: TObject);
begin
  if CurrentTile is TStaticItem then
  begin
    frmHueSettings.lbHue.ItemIndex := TStaticItem(CurrentTile).Hue;
    frmFilter.JumpToHue(TStaticItem(CurrentTile).Hue);
  end;
end;

procedure TfrmMain.mnuGrabFilterTileIDClick(Sender: TObject);
begin
  if CurrentTile is TStaticItem then
  begin
    frmFilter.AddTile(GetInternalTileID(CurrentTile));
    frmFilter.cbTileFilter.Checked := True;
    frmMain.InvalidateFilter;
  end;
end;

procedure TfrmMain.mnuGrabFilterHueClick(Sender: TObject);
begin
  if CurrentTile is TStaticItem then
  begin
    frmFilter.AddHue(TStaticItem(CurrentTile).Hue);
    frmFilter.cbHueFilter.Checked := True;
    frmMain.InvalidateFilter;
  end;
end;

procedure TfrmMain.mnuGrabVirtualLayerZClick(Sender: TObject);
begin
  frmVirtualLayer.seZ.Value := CurrentTile.Z;
  frmVirtualLayer.seZChange(frmVirtualLayer.seZ);
  if not frmVirtualLayer.cbShowLayer.Checked then
  begin
    frmVirtualLayer.cbShowLayer.Checked := True;
    frmMain.InvalidateScreenBuffer;
  end;
  //cursorNeedsUpdate := True;
  //Handled := True;
end;

procedure TfrmMain.GrabBoundaries(Sender: TObject);
begin
  if Sender = mnuGrabBoundMinZ then begin
    frmBoundaries.seMinZ.Value := CurrentTile.Z; frmBoundaries.seMinZChange(nil); end
  else if Sender = mnuGrabBoundMaxZ then begin
    frmBoundaries.seMaxZ.Value := CurrentTile.Z; frmBoundaries.seMaxZChange(nil); end
  else if Sender = mnuGrabBoundMinX then begin
    frmBoundaries.seMinX.Value := CurrentTile.X; frmBoundaries.seMinXChange(nil); end
  else if Sender = mnuGrabBoundMaxX then begin
    frmBoundaries.seMaxX.Value := CurrentTile.X; frmBoundaries.seMaxXChange(nil); end
  else if Sender = mnuGrabBoundMinY then begin
    frmBoundaries.seMinY.Value := CurrentTile.Y; frmBoundaries.seMinYChange(nil); end
  else if Sender = mnuGrabBoundMaxY then begin
    frmBoundaries.seMaxY.Value := CurrentTile.Y; frmBoundaries.seMaxYChange(nil); end;
end;

procedure TfrmMain.mnuLargeScaleCommandsClick(Sender: TObject);
begin
  frmLargeScaleCommand.Show;
end;

procedure TfrmMain.mnuRegionControlClick(Sender: TObject);
begin
  frmRegionControl.Show;
end;

procedure TfrmMain.mnuShowAnimationsClick(Sender: TObject);
begin
  FTextureManager.UseAnims := mnuShowAnimations.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.mnuShutdownClick(Sender: TObject);
begin
  dmNetwork.Send(TQuitServerPacket.Create(''));
end;



procedure TfrmMain.mnuWhiteBackgroundClick(Sender: TObject);
begin
  FRepaintNeeded := True;
end;

procedure TfrmMain.mnuWindowedModeClick(Sender: TObject);
var btn: Integer;
begin
  btn := MessageDlg(lbDlgWindowedModeSwitchCaption, lbDlgWindowedModeSwitch,
                                       mtConfirmation, mbYesNoCancel, 0);
    // btn=2=mbCancel; btn=6=mbYes; btn=7=mbNo;
    // Почему так хз, по идее btn=ord(mbCancel) или TMsgDlgBtn(btn)=mbCancel,
    // но этой чертовой среде ничего нравитсо ("Pascale must die")
  if btn = 2 then mnuWindowedMode.Checked := not mnuWindowedMode.Checked else begin
    XMLPropStorage1.Save;
    if btn = 6 then begin
      XMLPropStorage1.Restore;
      dmNetwork.Disconnect;
    end;
  end;
end;

procedure TfrmMain.mnuZoomClick(Sender: TObject);
var
  zoom : Single;
  cache: Integer;
begin

  if Sender = tbZoom then begin
    if tbZoom.Down then mnuZoomClick(mnuZoom100)
    else case tbZoom.Tag of
       250: mnuZoomClick(mnuZoom025);
       333: mnuZoomClick(mnuZoom033);
       500: mnuZoomClick(mnuZoom050);
       750: mnuZoomClick(mnuZoom075);
      1000: mnuZoomClick(mnuZoom100);
      1500: mnuZoomClick(mnuZoom150);
      2000: mnuZoomClick(mnuZoom200);
      3000: mnuZoomClick(mnuZoom300);
      4000: mnuZoomClick(mnuZoom400);
    end;
    exit;
  end;

  if (Sender as TMenuItem).Checked then begin
    if mnuZoom100 <> Sender then
      mnuZoomClick(mnuZoom100);
    exit;
  end;

  (Sender as TMenuItem).Checked := True;
  if mnuZoom025 <> Sender then mnuZoom025.Checked:= False else begin zoom:=0.250; cache:=4096; end;
  if mnuZoom033 <> Sender then mnuZoom033.Checked:= False else begin zoom:=0.333; cache:=2048; end;
  if mnuZoom050 <> Sender then mnuZoom050.Checked:= False else begin zoom:=0.500; cache:=1024; end;
  if mnuZoom075 <> Sender then mnuZoom075.Checked:= False else begin zoom:=0.750; cache:= 512; end;
  if mnuZoom100 <> Sender then mnuZoom100.Checked:= False else begin zoom:=1.000; cache:= 256; end;
  if mnuZoom150 <> Sender then mnuZoom150.Checked:= False else begin zoom:=1.500; cache:= 128; end;
  if mnuZoom200 <> Sender then mnuZoom200.Checked:= False else begin zoom:=2.000; cache:=  64; end;
  if mnuZoom300 <> Sender then mnuZoom300.Checked:= False else begin zoom:=3.000; cache:=  32; end;
  if mnuZoom400 <> Sender then mnuZoom400.Checked:= False else begin zoom:=4.000; cache:=  16; end;

  cache := 2 * Trunc(Sqrt(oglGameWindow.Width * oglGameWindow.Width +
           oglGamewindow.Height * oglGamewindow.Height) / (44 * zoom));
  cache := cache*cache;

  //cache := 4096;
  FLandscape.ResizeBlockCache(cache);

  if zoom < 1.0 then tbZoom.ImageIndex:= 37 else
  if zoom > 1.0 then tbZoom.ImageIndex:= 36 else
  tbZoom.ImageIndex:= 35;
  tbZoom.Down := zoom <> 1.0;
  if zoom <> 1.0 then
    tbZoom.Tag := Trunc(zoom * 1000.0);

  FRepaintNeeded := True;
  RebuildScreenBuffer;
end;

procedure TfrmMain.oglGameWindowDblClick(Sender: TObject);
begin
  if (acSelect.Checked) and (CurrentTile <> nil) then
    btnAddRandomClick(Sender);
end;

procedure TfrmMain.oglGameWindowKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var step, zoomfactor: Integer;
begin
  if tbZoom.Down then zoomfactor := tbZoom.Tag else zoomfactor := 1000;
  if Shift = [ssShift]
    then step := 1
    else if Shift = []
       then step := 8000 div zoomfactor
       else exit;

  case Key of
    VK_W, VK_NUMPAD8, VK_UP:    MoveBy(-step, -step);
    VK_S, VK_NUMPAD2, VK_DOWN:  MoveBy(+step, +step);
    VK_A, VK_NUMPAD4, VK_LEFT:  MoveBy(-step, +step);
    VK_D, VK_NUMPAD6, VK_RIGHT: MoveBy(+step, -step);
    VK_Q, VK_NUMPAD7:           MoveBy(-step, 0);
    VK_E, VK_NUMPAD9:           MoveBy(0, -step);
    VK_Y, VK_NUMPAD1:           MoveBy(0, +step);
    VK_G, VK_NUMPAD3:           MoveBy(+step, 0);
    VK_SPACE:                   if frmFilter.Visible then begin
                                   frmFilter.Locked := True;
                                   frmFilter.Hide;
                                   frmFilter.Locked := False;
                                end else frmFilter.Show;
  end;
end;

procedure TfrmMain.oglGameWindowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Logger.EnterMethod([lcClient, lcDebug], 'MouseDown');
  try
    if Button = mbRight then
      pmTools.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);

    if Button = mbMiddle then
      tmGrabTileInfoTimer(Sender);

    if Button <> mbLeft then
      Exit;

    UpdateCurrentTile(X, Y);
    if FOverlayUI.ActiveArrow > -1 then
      tmMovement.Enabled := True;

    SelectedTile := CurrentTile;
    if CurrentTile = nil then Exit;

    if acSelect.Checked then                      //***** Selection Mode *****//
      tmGrabTileInfo.Enabled := True;

    FRepaintNeeded := True;
  finally
    Logger.ExitMethod([lcClient, lcDebug], 'MouseDown');
  end;
end;

procedure TfrmMain.oglGameWindowMouseEnter(Sender: TObject);
begin
  if Active then
    oglGameWindow.SetFocus;

  FOverlayUI.Visible := True;

  if (frmFilter.Visible and mnuAutoShowFilterWindow.Checked) then
  begin
    frmFilter.Locked := True;
    frmFilter.Hide;
    frmFilter.Locked := False;
  end;

  FRepaintNeeded := True;
end;

procedure TfrmMain.oglGameWindowMouseLeave(Sender: TObject);
begin
  if not (frmConfirmation.Visible or
          (frmMoveSettings.Visible and (fsModal in frmMoveSettings.FormState))
         ) then //during confirmation the mouse would leave ...
  begin
    CurrentTile := nil;
    FOverlayUI.Visible := False;
  end;

  FRepaintNeeded := True;
end;

procedure TfrmMain.oglGameWindowMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  lastTile: TWorldItem;
  offsetX, offsetY: Integer;
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'MouseMove');
  lastTile := CurrentTile;
  
  if ssMiddle in Shift then
  begin
    UpdateCurrentTile(X, Y);
    if (lastTile <> nil) and (CurrentTile <> nil) and (lastTile <> CurrentTile) then
    begin
      offsetX := lastTile.X - CurrentTile.X;
      offsetY := lastTile.Y - CurrentTile.Y;
      if InRange(offsetX, -8, 8) and InRange(offsetY, -8, 8) then
        SetPos(FX - offsetX, FY - offsetY);
    end;
  end;

  UpdateCurrentTile(X, Y);

  FRepaintNeeded := True;
  //Logger.ExitMethod([lcClient, lcDebug], 'MouseMove');
end;

procedure TfrmMain.oglGameWindowMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  map: TMapCell;
  i: Integer;
  z: ShortInt;
  blockInfo: PBlockInfo;
  targetRect: TRect;
  offsetX, offsetY: Integer;
  item: TWorldItem;
  tileX, tileY, newX, newY: Word;
  targetBlocks: TBlockInfoList;  //а в чем разница с targetTiles: TWorldItemList; ?
  targetTile: TWorldItem;
begin
  Logger.EnterMethod([lcClient, lcDebug], 'MouseUp');
  if Button <> mbLeft then
  begin
    Logger.ExitMethod([lcClient, lcDebug], 'MouseUp');
    Exit;
  end;

  UpdateCurrentTile(X, Y);
  tmMovement.Enabled := False;
  if CurrentTile = nil then
  begin
    SelectedTile := nil;
    Logger.ExitMethod([lcClient, lcDebug], 'MouseUp');
    Exit;
  end;

  targetTile := CurrentTile;
  
  if acSelect.Checked then
  begin
    if tmGrabTileInfo.Enabled then
    begin
      tmGrabTileInfo.Enabled := False;
      mnuGrabTileIDClick(nil);
    end;

    for i := FSelectionListeners.Count - 1 downto 0 do
      FSelectionListeners[i](CurrentTile);
  end;

  if (not acSelect.Checked) and (targetTile <> nil) and (SelectedTile <> nil) then
  begin
    targetRect := GetSelectedRect;
    FUndoList := GetNextUndoList;

    if (SelectedTile = targetTile) or ConfirmAction then
    begin
      if acDraw.Checked then                        //***** Drawing Mode *****//
      begin
        for tileX := FSelection.Left-1 to FSelection.Right do
          for tileY := FSelection.Top-1 to FSelection.Bottom do
          begin
            map := FLandscape.MapCell[tileX, tileY];
            if map.IsGhost then
            begin
              FUndoList^.Add(TDrawMapPacket.Create(tileX, tileY, map.RawZ, map.RawTileID));
              dmNetwork.Send(TDrawMapPacket.Create(tileX, tileY, map.Z, map.TileID));
            end;
          end;

        Logger.Send([lcClient, lcDebug], 'Virtual tiles', FVirtualTiles.Count);
        for i := 0 to FVirtualTiles.Count - 1 do
        begin
          item := FVirtualTiles[i];
          if item is TGhostTile then
          begin
            dmNetwork.Send(TInsertStaticPacket.Create(item.X, item.Y, item.Z, item.TileID, TGhostTile(item).Hue));
            FUndoList^.Add(TDeleteStaticPacket.Create(TGhostTile(item)));
          end;
        end;
      end else if (SelectedTile <> targetTile) or targetTile.CanBeEdited then
      begin
        if (not acMove.Checked) or (SelectedTile <> targetTile) or
           (not frmMoveSettings.cbAsk.Checked) or ConfirmAction then
        begin
          targetBlocks := TBlockInfoList.Create;
          if SelectedTile = targetTile then
          begin
            blockInfo := nil;
            while FScreenBuffer.Iterate(blockInfo) do
            begin
              if blockInfo^.Item = targetTile then
              begin
                targetBlocks.Add(blockInfo);
                Break;
              end;
            end;
          end else
          begin
            blockInfo := nil;
            while FScreenBuffer.Iterate(blockInfo) do
            begin
              if (blockInfo^.State = ssNormal) and
                blockInfo^.Item.CanBeEdited and
                IsInRect(blockInfo^.Item.X, blockInfo^.Item.Y, targetRect) then
              begin
                targetBlocks.Add(blockInfo);
              end;
            end;
          end;

          if acMove.Checked then                       //***** Move item *****//
          begin
            offsetX := frmMoveSettings.GetOffsetX;
            offsetY := frmMoveSettings.GetOffsetY;

            if (frmMoveSettings.cbLand.Checked) then begin
              if offsetX >= 0 then newX := $7FFF else newX := 0;
              if offsetY >= 0 then newY := $7FFF else newY := 0;
              for i := 0 to targetBlocks.Count - 1 do if (targetBlocks.Items[i]^.Item is TMapCell) then begin
                if offsetX >= 0 then newX := min(newX, targetBlocks.Items[i]^.Item.X) else newX := max(newX, targetBlocks.Items[i]^.Item.X - 1);
                if offsetY >= 0 then newY := min(newY, targetBlocks.Items[i]^.Item.Y) else newY := max(newY, targetBlocks.Items[i]^.Item.Y - 1);
              end;

              tileX := 0;
              if offsetY > 0 then for i := newY to newY + offsetY do inc(tileX, FLandscape.MapCell[newX,i].RawZ+128) else
              if offsetY < 0 then for i := newY + offsetY to newY do inc(tileX, FLandscape.MapCell[newX,i].RawZ+128);
              if offsetX > 0 then for i := newX to newX + offsetX do inc(tileX, FLandscape.MapCell[i,newY].RawZ+128) else
              if offsetX < 0 then for i := newX + offsetX to newX do inc(tileX, FLandscape.MapCell[i,newY].RawZ+128);
              if (offsetX <> 0) and (offsetY <> 0)
                 then tileY := tileX div (abs(offsetX) + abs(offsetY) + 2)
                 else tileY := tileX div (abs(offsetX) + abs(offsetY) + 1);
              z := max(-128, min(tileY - 128, +127));
            end;

            for i := 0 to targetBlocks.Count - 1 do
            begin
              if frmMoveSettings.cbLand.Checked and (((offsetY > 0) or (offsetX > 0)) and not ((offsetY > 0) and (offsetX < 0)))
                then tileY := abs(i - targetBlocks.Count + 1) else tileY := i;
              item := targetBlocks.Items[tileY]^.Item;

              if (frmMoveSettings.cbItem.Checked) and (item is TStaticItem) then begin
                newX := EnsureRange(item.X + offsetX, 0, FLandscape.CellWidth - 1);
                newY := EnsureRange(item.Y + offsetY, 0, FLandscape.CellHeight - 1);
                FUndoList^.Add(TMoveStaticPacket.Create(newX, newY, item.Z, item.TileID, TStaticItem(item).Hue, item.X, item.Y));
                dmNetwork.Send(TMoveStaticPacket.Create(TStaticItem(item), newX, newY));
              end;
              if (frmMoveSettings.cbLand.Checked) and (item is TMapCell) then begin
                newX := EnsureRange(item.X + offsetX, 0, FLandscape.CellWidth - 1);
                newY := EnsureRange(item.Y + offsetY, 0, FLandscape.CellHeight - 1);
                map := FLandscape.MapCell[newX, newY];

                // Это не очень хорошо, для оптимизации следует ввести специальный пакет TMoveMapPacket
                FUndoList^.Add(TDrawMapPacket.Create(item.X, item.Y, item.Z, item.TileID));
                FUndoList^.Add(TDrawMapPacket.Create(newX, newY, map.RawZ, map.TileID));
                dmNetwork.Send(TDrawMapPacket.Create(item.X, item.Y, z, $0001));
                dmNetwork.Send(TDrawMapPacket.Create(newX, newY, item.Z, item.TileID));

              end;
            end;
          end else if acElevate.Checked then        //***** Elevate item *****//
          begin
            for i := 0 to targetBlocks.Count - 1 do
            begin
              item := targetBlocks.Items[i]^.Item;

              z := frmElevateSettings.seZ.Value;
              if frmElevateSettings.rbRaise.Checked then
                z := EnsureRange(item.Z + z, -128, 127)
              else if frmElevateSettings.rbLower.Checked then
                z := EnsureRange(item.Z - z, -128, 127);

              if item is TMapCell then
              begin
                if frmElevateSettings.cbRandomHeight.Checked then
                  Inc(z, Random(frmElevateSettings.seRandomHeight.Value));
                FUndoList^.Add(TDrawMapPacket.Create(item.X, item.Y, item.Z,
                  item.TileID));
                dmNetwork.Send(TDrawMapPacket.Create(item.X, item.Y, z,
                  item.TileID));
              end else
              begin
                FUndoList^.Add(TElevateStaticPacket.Create(item.X, item.Y,
                  z, item.TileID, TStaticItem(item).Hue, item.Z));
                dmNetwork.Send(TElevateStaticPacket.Create(TStaticItem(item), z));
              end;
            end;
          end else if acDelete.Checked then          //***** Delete item *****//
          begin
            Logger.Send([lcClient, lcDebug], 'targetBlocks.Count', targetBlocks.Count);
            for i := 0 to targetBlocks.Count - 1 do
            begin
              item := targetBlocks.Items[i]^.Item;
              if item is TStaticItem then
              begin
                FUndoList^.Add(TInsertStaticPacket.Create(item.X, item.Y,
                  item.Z, item.TileID, TStaticItem(item).Hue));
                dmNetwork.Send(TDeleteStaticPacket.Create(TStaticItem(item)));
              end;
            end;
          end else if acHue.Checked then                //***** Hue item *****//
          begin
            for i := 0 to targetBlocks.Count - 1 do
            begin
              blockInfo := targetBlocks.Items[i];
              item := blockInfo^.Item;

              if blockInfo^.HueOverride and (item is TStaticItem) then
              begin
                if TStaticItem(item).Hue <> blockInfo^.Hue then
                begin
                  FUndoList^.Add(THueStaticPacket.Create(item.X, item.Y, item.Z,
                    item.TileID, blockInfo^.Hue, TStaticItem(item).Hue));
                dmNetwork.Send(THueStaticPacket.Create(TStaticItem(item),
                    blockInfo^.Hue));
                end;
              end;
            end;
            blockInfo := nil;
          end;

          targetBlocks.Free;
        end;
      end;
    end;
  end;
  acUndo.Enabled := FUndoList^.Count > 0;
  SelectedTile := nil;
  FRepaintNeeded := True;
  Logger.ExitMethod([lcClient, lcDebug], 'MouseUp');
end;

procedure TfrmMain.oglGameWindowMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  cursorNeedsUpdate: Boolean;
  newZ: ShortInt;
begin
  //We want single steps ...
  WheelDelta := WheelDelta div WHEEL_DELTA;

  if ((Word(GetKeyState(VK_X)) and $8000)<>0) then begin
    if (Shift = [ssCtrl]) then begin
      frmBoundaries.seMinX.Value := frmBoundaries.seMinX.Value + min(max(WheelDelta,
          frmBoundaries.seMinX.MinValue-frmBoundaries.seMinX.Value),
          frmBoundaries.seMinX.MaxValue-frmBoundaries.seMinX.Value);
      frmBoundaries.seMinXChange(nil); exit; end
    else if (Shift = [ssShift]) then begin
      frmBoundaries.seMaxX.Value := frmBoundaries.seMaxX.Value + min(max(WheelDelta,
          frmBoundaries.seMaxX.MinValue-frmBoundaries.seMaxX.Value),
          frmBoundaries.seMaxX.MaxValue-frmBoundaries.seMaxX.Value);
      frmBoundaries.seMaxXChange(nil); exit; end;
  end else if ((Word(GetKeyState(VK_C)) and $8000)<>0) then begin
    if (Shift = [ssCtrl]) then begin
      frmBoundaries.seMinY.Value := frmBoundaries.seMinY.Value + min(max(WheelDelta,
          frmBoundaries.seMinY.MinValue-frmBoundaries.seMinY.Value),
          frmBoundaries.seMinY.MaxValue-frmBoundaries.seMinY.Value);
      frmBoundaries.seMinYChange(nil); exit; end
    else if (Shift = [ssShift]) then begin
      frmBoundaries.seMaxY.Value := frmBoundaries.seMaxY.Value + min(max(WheelDelta,
          frmBoundaries.seMaxY.MinValue-frmBoundaries.seMaxY.Value),
          frmBoundaries.seMaxY.MaxValue-frmBoundaries.seMaxY.Value);
      frmBoundaries.seMaxYChange(nil); exit; end;
  end else begin
    if (Shift = [ssCtrl]) then begin
      frmBoundaries.seMinZ.Value := frmBoundaries.seMinZ.Value + min(max(WheelDelta,
          frmBoundaries.seMinZ.MinValue-frmBoundaries.seMinZ.Value),
          frmBoundaries.seMinZ.MaxValue-frmBoundaries.seMinZ.Value);
      frmBoundaries.seMinZChange(nil); exit; end
    else if (Shift = [ssShift]) then begin
      frmBoundaries.seMaxZ.Value := frmBoundaries.seMaxZ.Value + min(max(WheelDelta,
          frmBoundaries.seMaxZ.MinValue-frmBoundaries.seMaxZ.Value),
          frmBoundaries.seMaxZ.MaxValue-frmBoundaries.seMaxZ.Value);
      frmBoundaries.seMaxZChange(nil); exit; end;
  end;


  if CurrentTile = nil then
    Exit;

  cursorNeedsUpdate := False;
  if (CurrentTile is TVirtualTile) or ((ssCtrl in Shift) and
     (frmVirtualLayer.cbShowLayer.Checked)) then
  begin
    frmVirtualLayer.seZ.Value := EnsureRange(frmVirtualLayer.seZ.Value +
      WheelDelta, -128, 127);
    frmVirtualLayer.seZChange(frmVirtualLayer.seZ);
    cursorNeedsUpdate := True;
    Handled := True;
  end else if not (ssCtrl in Shift) then
  begin
    FUndoList := GetNextUndoList;
    newZ := EnsureRange(CurrentTile.Z + WheelDelta, -128, 127);
    if CurrentTile is TStaticItem then
    begin
      FUndoList^.Add(TElevateStaticPacket.Create(CurrentTile.X, CurrentTile.Y,
        newZ, CurrentTile.TileID, TStaticItem(CurrentTile).Hue,
        CurrentTile.Z));
      dmNetwork.Send(TElevateStaticPacket.Create(TStaticItem(CurrentTile),
        newZ));
      cursorNeedsUpdate := True;
      Handled := True;
    end else if CurrentTile is TMapCell then
    begin
      FUndoList^.Add(TDrawMapPacket.Create(CurrentTile.X, CurrentTile.Y,
        CurrentTile.Z, CurrentTile.TileID));
      dmNetwork.Send(TDrawMapPacket.Create(CurrentTile.X, CurrentTile.Y,
        newZ, CurrentTile.TileID));
      Handled := True;
    end;
    acUndo.Enabled := FUndoList^.Count > 0;
  end;
  
  if cursorNeedsUpdate then
  begin
    SetCursorPos(Mouse.CursorPos.X, Mouse.CursorPos.Y - 4 * WheelDelta);
    UpdateCurrentTile(MousePos.X, MousePos.Y - 4 * WheelDelta);
  end;

  FRepaintNeeded := True;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i : Integer;
  ARegistry: TRegistry;
begin
  Logger.EnterMethod([lcLandscape, lcDebug], 'TfrmMain.FormCreate(Sender: TObject)');

  // Подменяем TVirtualDrawTree на его перегруженный аналог TVirtualList
  vdtTiles := TVirtualList.Create(vdtTiles);
  vdtRandom := TVirtualList.Create(vdtRandom);
  vdlRandom := TVirtualList(vdtRandom); // Лазареусу пчмуто не нравиться если поменять тип у vdtRandom

  oglGameWindow.Cursor := +01;
  pcLeft.ActivePageIndex  := tsNavigation.PageIndex;

  LanguageTranslate(Self);
  Application.TaskBarBehavior := tbSingleButton;

  {
  BorderStyle := bsNone;
  FormStyle   := fsStayOnTop;
  WindowState := wsMaximized;
  Left:=0; Top:=0;
  Width:=1920;
  Height:=1200; }
  //oglGameWindow.Visible:= false;
  //oglGameWindowResize(Sender);

  FConfigDir := GetAppConfigDir(False);

  (*ARegistry := TRegistry.Create();
  ARegistry.RootKey := HKEY_LOCAL_MACHINE;
  ARegistry.OpenKey('\SOFTWARE\Quintessence\UO CentrED+', False);

  FAppDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  FLocalDir := FAppDir + '..' + PathDelim + 'LocalData' + PathDelim;
  if ARegistry.ReadBool('UseConfigDir')
    then FConfigDir := GetAppConfigDir(False)
    else FConfigDir := FLocalDir + 'UsersData' + PathDelim;
  ARegistry.Free;*) //TODO cross platform

  if (sprofile <> '')
    then FProfileDir := FConfigDir + 'Profiles' + PathDelim + UTF8ToCP1251(sprofile) + PathDelim
    else FProfileDir := '';
  ForceDirectories(FConfigDir);

  if (FProfileDir <> '')
    then XMLPropStorage1.FileName := FProfileDir + 'Config.xml'
    else XMLPropStorage1.FileName := FConfigDir + 'Config.xml';
  XMLPropStorage1.Active := True;

  FLandscape := ResMan.Landscape;
  FLandscape.OnChange := @OnLandscapeChanged;
  FLandscape.OnMapChanged := @OnMapChanged;
  FLandscape.OnNewBlock := @OnNewBlock;
  FLandscape.OnStaticDeleted := @OnStaticDeleted;
  FLandscape.OnStaticElevated := @OnStaticElevated;
  FLandscape.OnStaticHued := @OnStaticHued;
  FLandscape.OnStaticInserted := @OnStaticInserted;

  Logger.Send([lcClient, lcInfo], 'LoadNoDrawMap()...');
  if FileExists(FProfileDir + 'VirtualTiles.xml') then
    FLandscape.LoadNoDrawMap(FProfileDir + 'VirtualTiles.xml')
  else begin
    if FileExists(FLocalDir + 'VirtualTiles.xml') then
      FLandscape.LoadNoDrawMap(FLocalDir + 'VirtualTiles.xml');
    if FileExists(FConfigDir + 'VirtualTiles.xml') then
      FLandscape.LoadNoDrawMap(FConfigDir + 'VirtualTiles.xml');
    if FileExists(ResMan.GetFile('VirtualTiles.xml')) then
      FLandscape.LoadNoDrawMap(ResMan.GetFile('VirtualTiles.xml'));
  end;

  FTextureManager := TLandTextureManager.Create;
  FScreenBuffer := TScreenBuffer.Create;
  FScreenBufferState := [];
  X := 0;
  Y := 0;
  edX.MaxValue := FLandscape.CellWidth;
  edY.MaxValue := FLandscape.CellHeight;
  FOverlayUI := TOverlayUI.Create;
  FLightManager := TLightManager.Create(@GetDrawOffset);
  
  ProcessAccessLevel;

  Logger.Send([lcClient, lcInfo], 'LoadLightSourceTiles()...');
  if FileExists(FProfileDir + 'VirtualTiles.xml') then
    LoadLightSourceTiles (FProfileDir + 'VirtualTiles.xml')
  else begin
    if FileExists(FLocalDir + 'VirtualTiles.xml') then
      LoadLightSourceTiles (FLocalDir + 'VirtualTiles.xml');
    if FileExists(FConfigDir + 'VirtualTiles.xml') then
      LoadLightSourceTiles (FConfigDir + 'VirtualTiles.xml');
    if FileExists(ResMan.GetFile('VirtualTiles.xml')) then
      LoadLightSourceTiles (ResMan.GetFile('VirtualTiles.xml'));
  end;

  Logger.Send([lcClient, lcInfo], 'LoadVisibleTiles()...');
  if FileExists(FProfileDir + 'VirtualTiles.xml') then
    LoadVisibleTiles(FProfileDir + 'VirtualTiles.xml')
  else begin
    if FileExists(FLocalDir + 'VirtualTiles.xml') then
      LoadVisibleTiles(FLocalDir + 'VirtualTiles.xml');
    if FileExists(FConfigDir + 'VirtualTiles.xml') then
      LoadVisibleTiles(FConfigDir + 'VirtualTiles.xml');
    if FileExists(ResMan.GetFile('VirtualTiles.xml')) then
      LoadVisibleTiles(ResMan.GetFile('VirtualTiles.xml'));
  end;

  Logger.Send([lcClient, lcInfo], 'LoadColorLight()...');
  if FileExists(FProfileDir + 'ColorLight.xml') then
    FLightManager.LoadConfig(FProfileDir + 'ColorLight.xml')
  else begin
    if FileExists(FLocalDir + 'ColorLight.xml') then
      FLightManager.LoadConfig(FLocalDir + 'ColorLight.xml');
    if FileExists(FConfigDir + 'ColorLight.xml') then
      FLightManager.LoadConfig(FConfigDir + 'ColorLight.xml');
    if FileExists(ResMan.GetFile('ColorLight.xml')) then
      FLightManager.LoadConfig(ResMan.GetFile('ColorLight.xml'));
  end;

  FTilesSelectionUndoRedoCommandGroup  := TUndoRedoCommandGroup.Create;
  FGroupsSelectionUndoRedoCommandGroup := TUndoRedoCommandGroup.Create;
  FTilesSelectionUndoRedoManager  := TUndoRedoManager.Create;
  FGroupsSelectionUndoRedoManager := TUndoRedoManager.Create;
  tvGroups.NodeDataSize := SizeOf(TGroupNode);
  vdtTiles.NodeDataSize := SizeOf(TTileInfo);
  vdlRandom.NodeDataSize := SizeOf(TTileInfo);
  //mnuTileListViewClick(nil);
  LoadEntryTilesList;
  LoadBrushTilesList;
  LoadSurfsTilesList;
  BuildGroupList;
  BuildTileList;
  Randomize;
  
  vstChat.NodeDataSize := SizeOf(TChatInfo);
  pnlChatHeader.AnchorSide[akBottom].Control := pcLeft;
  pnlChatHeader.AnchorSide[akBottom].Side := asrBottom;

  if FProfileDir <> ''
    then FLocationsFile := FProfileDir + 'Locations.xml'
    else FLocationsFile := FConfigDir + 'Locations.xml';
  vstLocations.NodeDataSize := SizeOf(TLocationInfo);
  LoadLocations;

  vstClients.NodeDataSize := SizeOf(TClientInfo);

  Logger.Send([lcClient, lcInfo], 'RegisterPacketHandler()...');
  RegisterPacketHandler($0C, TPacketHandler.Create(0, @OnClientHandlingPacket));

  FVLayerImage := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/VirtualLayer.tga'));
  FVLightSrcImage[ 1] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24nn.tga'));
  FVLightSrcImage[ 2] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24bw.tga'));
  FVLightSrcImage[ 3] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24tw.tga'));
  FVLightSrcImage[ 4] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24bo.tga'));
  FVLightSrcImage[ 5] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24to.tga'));
  FVLightSrcImage[ 6] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24by.tga'));
  FVLightSrcImage[ 7] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24ty.tga'));
  FVLightSrcImage[ 8] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24ny.tga'));
  FVLightSrcImage[ 9] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24bl.tga'));
  FVLightSrcImage[10] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24bb.tga'));
  FVLightSrcImage[11] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24tb.tga'));
  FVLightSrcImage[12] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24bg.tga'));
  FVLightSrcImage[13] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24tg.tga'));
  FVLightSrcImage[14] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24br.tga'));
  FVLightSrcImage[15] := TSingleImage.CreateFromStream(ResourceManager.GetResource('Overlay/LightBulb_24bp.tga'));

  FGLFont := TGLFont.Create;
  FGLFont.LoadImage(ResourceManager.GetResource('GLFont/DejaVu.png'));
  FGLFont.LoadFontInfo(ResourceManager.GetResource('GLFont/DejaVu.fnt'));

  Logger.Send([lcClient, lcInfo], 'TWorldItemList.Create(True)...');
  FVirtualTiles := TWorldItemList.Create(True);

  for i:=1 to FUndoListLength do
    FUndoListArray[i] := TPacketList.Create(True);
  FUndoList := @FUndoListArray[1];
  FUndoListFirstIndex := 0;
  FundoListLastIndex  := 0;

  if FProfileDir <> ''
    then FRandomPresetsFile := FProfileDir + 'RandomPresets.xml'
    else FRandomPresetsFile := FConfigDir + 'RandomPresets.xml';
  LoadRandomPresets;

  Logger.Send([lcClient, lcInfo], 'Завершение загрузки...');
  DoubleBuffered := True;
  //pnlBottom.DoubleBuffered := True;

  FAccessChangedListeners := TAccessChangedListeners.Create;
  FSelectionListeners := TSelectionListeners.Create;
  
  FLastDraw := Now;

  Logger.ExitMethod([lcLandscape, lcDebug], 'TfrmMain.FormCreate(Sender: TObject)');
end;

procedure TfrmMain.btnGoToClick(Sender: TObject);
begin
  SetPos(edX.Value, edY.Value);
end;

procedure TfrmMain.btnRandomPresetDeleteClick(Sender: TObject);
var
  preset: TDOMElement;
begin
  if cbRandomPreset.ItemIndex > -1 then
  begin
    preset := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    FRandomPresetsDoc.DocumentElement.RemoveChild(preset);
    cbRandomPreset.Items.Delete(cbRandomPreset.ItemIndex);
    cbRandomPreset.ItemIndex := -1;
  end;
end;

procedure TfrmMain.btnRandomPresetSaveClick(Sender: TObject);
var
  presetName: string;
  i: Integer;
  presetElement, tileElement: TDOMElement;
  children: TDOMNodeList;
  tileNode: PVirtualItem;
  tileInfo: PTileInfo;
begin
  presetName := cbRandomPreset.Text;
  if InputQuery(lbDlgSaveRandPrsCaption, lbDlgSaveRandPrs, presetName) then
  begin
    presetElement := FindRandomPreset(presetName);
    if presetElement = nil then
    begin
      presetElement := FRandomPresetsDoc.CreateElement('PresetElement');
      presetElement.AttribStrings['Name'] := UTF8ToCP1251(presetName);
      FRandomPresetsDoc.DocumentElement.AppendChild(presetElement);
      cbRandomPreset.Items.AddObject(presetName, presetElement);
    end else
    begin
      children := presetElement.ChildNodes;
      for i := children.Count - 1 downto 0 do
        presetElement.RemoveChild(children[i]);
    end;

    tileNode := vdlRandom.GetFirst;
    while tileNode <> nil do
    begin
      tileInfo := vdlRandom.GetNodeData(tileNode);
      tileElement := FRandomPresetsDoc.CreateElement('TileElement');
      tileElement.AttribStrings['ID'] := IntToStr(tileInfo^.ID);
      presetElement.AppendChild(tileElement);
      tileNode := vdlRandom.GetNext(tileNode);
    end;

    cbRandomPreset.ItemIndex := cbRandomPreset.Items.IndexOfObject(presetElement);

    SaveRandomPresets;
  end;
end;

procedure TfrmMain.cbRandomPresetChange(Sender: TObject);
var
  presetElement, tileElement: TDOMElement;
  tiles: TDOMNodeList;
  tileNode: PVirtualItem;
  tileInfo: PTileInfo;
  i, id: Integer;
begin
  if cbRandomPreset.ItemIndex > -1 then
  begin
    vdlRandom.Clear;
    presetElement := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    tiles := presetElement.ChildNodes;
    for i := 0 to tiles.Count - 1 do
    begin
      tileElement := TDOMElement(tiles[i]);
      if (tileElement.NodeName = 'TileElement') and
         TryStrToInt(tileElement.AttribStrings['ID'], id) and
         (id < FLandscape.MaxStaticID + $4000) then
      begin
        tileNode := vdlRandom.AddItem(nil);
        tileInfo := vdlRandom.GetNodeData(tileNode);
        tileInfo^.ID := id;
      end;
    end;
  end;
end;

procedure TfrmMain.btnAddRandomClick(Sender: TObject);
var
  selected: PVirtualItem;
  node: PVirtualItem;
  sourceTileInfo, targetTileInfo: PTileInfo;
begin
  vdlRandom.BeginUpdate;
  selected := vdtTiles.GetFirstSelected;
  while selected <> nil do
  begin
    sourceTileInfo := vdtTiles.GetNodeData(selected);
    node := vdlRandom.AddItem(nil);
    targetTileInfo := vdlRandom.GetNodeData(node);
    targetTileInfo^.ID  := sourceTileInfo^.ID;
    targetTileInfo^.ptr := sourceTileInfo^.ptr;
    selected := vdtTiles.GetNextSelected(selected);
  end;
  vdlRandom.EndUpdate;
end;

procedure TfrmMain.btnClearLocationsClick(Sender: TObject);
begin
  if MessageDlg(lbDlgDelConfCaption, lbDlgDelConf,
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    vstLocations.Clear;
  end;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if (FScreenBufferState <> CScreenBufferValid) or
     ((FRepaintNeeded or mnuShowAnimations.Checked) and
      (MilliSecondsBetween(Now, FLastDraw) > 50)) then
  begin
    //Logger.Send([lcClient, lcDebug], 'Repainting Game Window');
    oglGameWindow.Repaint;
    FLastDraw := Now;
    FRepaintNeeded := False;
  end;
  Sleep(1);
  Done := False;
end;

procedure TfrmMain.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  //that check is a bit dirty, but serves its purpose
  //(i.e. to set the timeout for the tile info hints)
  if HintStr = '-' then
    HintInfo.HideTimeout := Application.HintHidePause +
      Application.HintHidePausePerChar * (Length(FTileHint.Name) +
      Length(FTileHint.Obj) + Length(FTileHint.Flags));
end;

procedure TfrmMain.btnAddLocationClick(Sender: TObject);
var
  locationName: string;
  locationInfo: PLocationInfo;
begin
  locationName := '';
  if InputQuery(lbDlgNewQuerryCaption, lbDlgNewQuerry,
    locationName) then
  begin
    locationInfo := vstLocations.GetNodeData(vstLocations.AddChild(nil));
    locationInfo^.X := X;
    locationInfo^.Y := Y;
    locationInfo^.Name := locationName;
  end;
end;

procedure TfrmMain.acSelectExecute(Sender: TObject);
begin
  acSelect.Checked := True;
  tbSelect.Down := True;
  mnuSelect.Checked := True;
  ProcessToolState;
end;

procedure TfrmMain.acSelectionExecute(Sender: TObject);
begin
  acSelection.Checked := True;
  tbSelection.Down := True;
  mnuSelection.Checked := True;
  frmSelectionSettings.Show;
  ProcessToolState;
end;

function TfrmMain.GetNextUndoList: PPacketList;
var
  i: Integer;
begin
  Inc(FUndoListLastIndex, 1);
  if FUndoListLastIndex > FUndoListLength
     then FUndoListLastIndex := 1;

  if (FUndoListFirstIndex = FUndoListLastIndex) or (FUndoListFirstIndex = 0)
    then Inc(FUndoListFirstIndex, 1);
  if FUndoListFirstIndex > FUndoListLength
    then FUndoListFirstIndex := 1;

  if FUndoListFirstIndex <= FUndoListLastIndex
    then i := FUndoListLastIndex - FUndoListFirstIndex + 1
    else i := FUndoListLastIndex + FUndoListLength - FUndoListFirstIndex + 1;
  tbUndo.Hint := lbToolbarUndo + ' (' + IntToStr(i) + ').';

  FUndoListArray[FundoListLastIndex].Clear;
  GetNextUndoList := @FUndoListArray[FundoListLastIndex];
end;

procedure TfrmMain.acUndoExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := FUndoList^.Count - 1 downto 0 do
  begin
    dmNetwork.Send(FUndoList^[i]);
    FUndoList^[i] := nil;
  end;
  FUndoList^.Clear;

  if FUndoListLastIndex = FUndoListFirstIndex then
    begin
      FUndoListFirstIndex:= 0;
      FUndoListLastIndex := 0;
      tbUndo.Hint := lbToolbarUndo + ' (0).';
      acUndo.Enabled := False;
    end
  else
    begin
      Inc(FUndoListLastIndex, -1);
      if FUndoListLastIndex = 0
        then FUndoListLastIndex := FUndoListLength;
      FUndoList := @FUndoListArray[FundoListLastIndex];
      if FUndoListFirstIndex <= FUndoListLastIndex
        then i := FUndoListLastIndex - FUndoListFirstIndex + 1
        else i := FUndoListLastIndex + FUndoListLength - FUndoListFirstIndex + 1;
      tbUndo.Hint := lbToolbarUndo + ' (' + IntToStr(i) + ').';
    end;
end;

procedure TfrmMain.acVirtualLayerExecute(Sender: TObject);
begin
  frmVirtualLayer.Show;
end;

procedure TfrmMain.acTerrainExecute(Sender: TObject);
begin
  acTerrain.Checked := not acTerrain.Checked;
  //tbTerrain.Down := acTerrain.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.acStaticsExecute(Sender: TObject);
begin
  acStatics.Checked := not acStatics.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.acWalkableExecute(Sender: TObject);
begin
  InvalidateFilter;
  FRepaintNeeded := True;
end;

procedure TfrmMain.acDrawExecute(Sender: TObject);
begin
  acDraw.Checked := True;
  tbDrawTile.Down := True;
  mnuDraw.Checked := True;
  frmDrawSettings.Show;
  ProcessToolState;
end;

procedure TfrmMain.acFillExecute(Sender: TObject);
begin
  acFill.Checked := True;
  tbFill.Down := True;
  mnuFill.Checked := True;
  frmFillSettings.Show;
  ProcessToolState;
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
begin
  acDelete.Checked := True;
  tbDeleteTile.Down := True;
  mnuDelete.Checked := True;
  ProcessToolState;
end;

procedure TfrmMain.acBoundariesExecute(Sender: TObject);
begin
  frmBoundaries.Show;
end;

procedure TfrmMain.acElevateExecute(Sender: TObject);
begin
  acElevate.Checked := True;
  tbElevateTile.Down := True;
  mnuElevate.Checked := True;
  ProcessToolState;
  frmElevateSettings.Show;
end;



procedure TfrmMain.acSurfElevateExecute(Sender: TObject);
begin
  acSurfElevate.Checked := True;
  tbSurfElevate.Down := True;
  mnuSurfElevate.Checked := True;
  frmSurfElevateSettings.Show;
  ProcessToolState;
end;

procedure TfrmMain.acSurfStretchExecute(Sender: TObject);
begin
  acSurfStretch.Checked := True;
  tbSurfStretch.Down := True;
  mnuSurfStretch.Checked := True;
  frmSurfStretchSettings.Show;
  ProcessToolState;
end;

procedure TfrmMain.acSurfSmoothExecute(Sender: TObject);
begin
  acSurfSmooth.Checked := True;
  tbSurfSmooth.Down := True;
  mnuSurfSmooth.Checked := True;
  frmSurfSmoothSettings.Show;
  ProcessToolState;
end;

procedure TfrmMain.acFilterExecute(Sender: TObject);
begin
  if acFilter.Checked then
  begin
    frmFilter.Show;
    frmFilter.Locked := False;
    if (tbFilter.Down) then begin
      frmFilter.tFormClose.Interval := 1500;
      frmFilter.tFormClose.Tag := PtrInt(True);
      frmFilter.tFormClose.Enabled := True;
    end;
  end else
    frmFilter.Hide;
  InvalidateFilter;
end;

procedure TfrmMain.acFlatExecute(Sender: TObject);
begin
  acFlat.Checked := not acFlat.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.acHueExecute(Sender: TObject);
begin
  acHue.Checked := True;
  tbSetHue.Down := True;
  mnuSetHue.Checked := True;
  ProcessToolState;
  frmHueSettings.Show;
end;

procedure TfrmMain.acLightlevelExecute(Sender: TObject);
begin
  frmLightlevel.Show;
end;

procedure TfrmMain.acMoveExecute(Sender: TObject);
begin
  acMove.Checked := True;
  tbMoveTile.Down := True;
  mnuMove.Checked := True;
  ProcessToolState;
  frmMoveSettings.Show;
end;

procedure TfrmMain.acNoDrawExecute(Sender: TObject);
begin
  acNoDraw.Checked := not acNoDraw.Checked;
  RebuildScreenBuffer;
end;

procedure TfrmMain.btnClearRandomClick(Sender: TObject);
begin
  vdlRandom.BeginUpdate;
  vdlRandom.Clear;
  vdlRandom.EndUpdate;
end;

procedure TfrmMain.btnDeleteLocationClick(Sender: TObject);
begin
  vstLocations.DeleteSelectedNodes;
end;

procedure TfrmMain.btnDeleteRandomClick(Sender: TObject);
begin
  vdlRandom.BeginUpdate;
  vdlRandom.DeleteSelectedNodes;
  vdlRandom.EndUpdate;
end;

procedure TfrmMain.cbStaticsChange(Sender: TObject);
begin
  //tvGroups.Enabled := not (cbStatics.Checked or cbTerrain.Checked);
  if mnuAutoHideGroupList.Checked then begin
    if (((cbStatics.Checked) or (cbTerrain.Checked)) = spGroupList.Enabled)
      then mnuAutoHideGroupListClick(Sender);
  end else if (cbStatics.Checked or cbTerrain.Checked)
    then tvGroups.ClearSelection;

  //if (not cbStatics.Checked) and (not cbTerrain.Checked) then
  //  cbTerrain.Checked := True;

  //if tvGroups.Selected <> nil then
  //  tvGroups.Selected := nil
  //else
    BuildTileList;
end;

procedure TfrmMain.cbTerrainChange(Sender: TObject);
begin
  if mnuAutoHideGroupList.Checked then
    if (((cbStatics.Checked) or (cbTerrain.Checked)) = spGroupList.Enabled)
      then mnuAutoHideGroupListClick(Sender);
  //if (not cbTerrain.Checked) and (not cbStatics.Checked) then
  //  cbStatics.Checked := True;

  //if tvGroups.Selected <> nil then
  //  tvGroups.Selected := nil
  //else
    BuildTileList;
end;

procedure TfrmMain.tvSelectGroupsChanged(Sender: TObject);
begin
  if (not cbTerrain.Checked) and (not cbStatics.Checked) then
     BuildTileList;
 // if Sender = cbStatics then
   // MessageDlg('Ошибка', 'Тайл с указанным ID не был найден.' +
     //   LineEnding + 'Проверте конфликты настроек фильтров.', mtError, [mbOK], 0);
  if cbTerrain.Checked then cbTerrain.Checked := False;
  if cbStatics.Checked then cbStatics.Checked := False;
end;

procedure TfrmMain.mnuReloadGroupsClick(Sender: TObject);
var
  fPath : string;
begin
  FreeGroupLists;
  LoadEntryTilesList;
  LoadBrushTilesList;
  LoadSurfsTilesList;
  BuildGroupList;

  if FileExists(FProfileDir + 'ColorLight.xml')
    then fPath := (FProfileDir + 'ColorLight.xml')
    else if FileExists(FLocalDir + 'ColorLight.xml')
      then fPath := (FLocalDir + 'ColorLight.xml')
      else if FileExists(FConfigDir + 'ColorLight.xml')
        then fPath := (FConfigDir + 'ColorLight.xml')
        else if FileExists(ResMan.GetFile('ColorLight.xml'))
          then fPath := (ResMan.GetFile('ColorLight.xml'))
          else Exit;
  FLightManager.LoadConfig(fPath);

  if FileExists(FProfileDir + 'VirtualTiles.xml')
    then fPath := (FProfileDir + 'VirtualTiles.xml')
    else if FileExists(FLocalDir + 'VirtualTiles.xml')
      then fPath := (FLocalDir + 'VirtualTiles.xml')
      else if FileExists(FConfigDir + 'VirtualTiles.xml')
        then fPath := (FConfigDir + 'VirtualTiles.xml')
        else if FileExists(ResMan.GetFile('VirtualTiles.xml'))
          then fPath := (ResMan.GetFile('VirtualTiles.xml'))
          else Exit;
  LoadLightSourceTiles(fPath);
  LoadVisibleTiles(fPath);
end;

procedure TfrmMain.mnuSetLanguageClick(Sender: TObject);
var
  settings: TIniFile;
begin
  LanguageSet(Integer(TMenuItem(Sender).Tag));
  ReloadLanguageTranslation();
  frmMain.Menu := nil; // Перерисовываем главное меню
  frmMain.Menu := MainMenu1;
  ProcessAccessLevel;  // Обновляем заголовок формы

  // Запоминаем выбранный язык
  settings := TIniFile.Create(FConfigDir + 'LoginSettings.ini');
  settings.WriteString('Profile', 'Lang', TMenuItem(Sender).Caption);
  settings.Free;
end;

//procedure ssrSaveHandle(handle: HWND; filepath: PChar);  external  'SSRender';
  //function GetDesktopWindow(): HWND; external  'SSRender';
//function GetDesktopWindow:HWND; external 'user32' name 'GetDesktopWindow';
procedure TfrmMain.mnuMakeScreenShotClick(Sender: TObject);
var
  Bitmap: TBitmap;
  Image: TJPEGImage;
  ScreenWND: HWND;
  ScreenDC: HDC;
  DateTime: TDateTime;
  FolderPath: string;
  Refocus: Boolean;
  srcRect, destRect: TRect;
  oglPoint: TPoint;
begin
  Bitmap:= TBitmap.Create;
  Image := TJPEGImage.Create;
  // We remove the focus from the window (to remove the arrows)
  Refocus := oglGameWindow.Focused;
  if Refocus then begin
    oglGameWindowMouseLeave(Sender);
    oglGameWindow.Paint;
  end;
  // Obtaining device context
  oglGameWindow.HandleNeeded;
  //TODO : ScreenWND := GetDesktopWindow();// oglGameWindow.Handle; //Handle;//0;//
  ScreenDC := GetDC(ScreenWND);   //GetDeviceContext(ScreenWND);
  if ScreenDC<>0 then try
    // We get the "buffer" screen
    Bitmap.LoadFromDevice(ScreenDC);
    // The size of the stored image
    destRect.Left  :=0;
    destRect.Top   :=0;
    destRect.Right :=oglGameWindow.Width;
    destRect.Bottom:=oglGameWindow.Height;
    Image.SetSize(destRect.Right, destRect.Bottom);
    // Keep the area on the screen
    oglPoint.x := 0;  oglPoint.y := 0;
    oglPoint := oglGameWindow.ClientToScreen(oglPoint);
    srcRect.Left   := oglPoint.x;
    srcRect.Top    := oglPoint.y;
    srcRect.Right  := srcRect.Left + destRect.Right;
    srcRect.Bottom := srcRect.Top  + destRect.Bottom;
    // Copy the area from the screen to the stored image
    Image.Canvas.CopyRect(destRect, Bitmap.Canvas, srcRect);
    // Save the file
    DateTime := Now;
    FolderPath := FAppDir + '../ScreenShots/' + FormatDateTime('YYYY-MM-DD', DateTime) + '/';
    ForceDirectories(FolderPath);
    Image.SaveToFile(FolderPath + FormatDateTime('HHNN-SSZZZ', DateTime) + '.jpg');
    WriteChatMessage('System', Format('%s "./../ScreenShots/%s/%s.jpg"',
    [lbScreenShotMsg, FormatDateTime('YYYY-MM-DD', DateTime), FormatDateTime('HHNN-SSZZZ', DateTime)]));
  finally
    if ReleaseDC(ScreenWND, ScreenDC)<>1 then
      MessageDlg(lbDlgFreeDcErrCaption, lbDlgFreeDcErr, mtError, [mbOK], 0);
    Bitmap.Free;
    Image.Free;
  end else
    MessageDlg(lbDlgGetDcErrCaption, lbDlgGetDcErr, mtError, [mbOK], 0);
  // Return back to the focus window (if necessary)
  if Refocus then begin
    oglGameWindowMouseEnter(Sender);
    oglGameWindow.Paint;
  end;

  //DateTime := Now;
  //FolderPath := FAppDir + 'ScreenShots/' + FormatDateTime('YYYY-MM-DD', DateTime) + '/';
  //ForceDirectories(FolderPath);
  //MessageDlg('Ошибка', Format('Handle: %d File: "%s"', [ScreenWND, FolderPath + FormatDateTime('HHNN-SSZZZ', DateTime) + '.bmp']), mtError, [mbOK], 0);

  //ssrSaveHandle(ScreenWND, PChar(FolderPath + FormatDateTime('HHNN-SSZZZ', DateTime) + '.bmp'));
  //oglGameWindowResize(Sender);
end;

procedure TfrmMain.edChatKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    if edChat.Text <> '' then
    begin
      dmNetwork.Send(TChatMessagePacket.Create(edChat.Text));
      edChat.Text := '';
    end;
  end;
end;

procedure TfrmMain.edFilterEditingDone(Sender: TObject);
var
  chrtemp: char;
  strtemp: string;
  inttemp: Integer;
begin
  strtemp := edFilter.Text;
  if (Length(strtemp) > 2) and (strtemp[1] = '0') and ((strtemp[2] = 'x') or (strtemp[2] = 'X'))
    then begin Delete(strtemp, 1, 1); strtemp[1] := '$'; end;
  if (TryStrToInt(strtemp, inttemp)) then begin
    edSearchID.Text := strtemp;
    chrtemp := #13;
    Self.edSearchIDKeyPress(nil, chrtemp);
    edSearchID.Text := '';
    edFilter.Text := '';
    Exit;
  end;

  BuildTileList;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  Logger.EnterMethod([lcLandscape, lcDebug], 'TfrmMain.FormActivate(Sender: TObject)');
  if oglGameWindow.MouseEntered then
    oglGameWindowMouseEnter(Sender);
  Logger.ExitMethod([lcLandscape, lcDebug], 'TfrmMain.FormActivate(Sender: TObject)');
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  frmDrawSettings.rbTileList.Checked := True;
  frmDrawSettings.rbRandom.Checked := False;
  dmNetwork.CheckClose(Self);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  CurrentTile := nil;
  SelectedTile := nil;

  SaveLocations;
  SaveRandomPresets;

  FreeAndNil(FTextureManager);
  FreeAndNil(FScreenBuffer);
  FreeAndNil(FOverlayUI);
  FreeAndNil(FLightManager);
  FreeAndNil(FVLayerImage);
  FreeAndNil(FVLayerMaterial);
  for i:=1 to FVLightSrcImageCount do
      FreeAndNil(FVLightSrcImage[i]);
  if FVLightSrcMaterial <> nil then begin
      for i := 0 to FVLightSrcImageCount - 1 do
          FreeAndNil(FVLightSrcMaterial[i]);
      freemem(FVLightSrcMaterial);
      FVLightSrcMaterial := nil;
  end;
  FreeAndNil(FVirtualTiles);
  for i:=1 to FUndoListLength do
    FreeAndNil(FUndoListArray[i]);
  FreeAndNil(FGLFont);
  FreeAndNil(FRandomPresetsDoc);
  FreeAndNil(FAccessChangedListeners);
  FreeAndNil(FSelectionListeners);

  FreeAndNil(FTilesSelectionUndoRedoManager);
  FreeAndNil(FGroupsSelectionUndoRedoManager);
  FreeAndNil(FTilesSelectionUndoRedoCommandGroup);
  FreeAndNil(FGroupsSelectionUndoRedoCommandGroup);

  RegisterPacketHandler($0C, nil);
end;

procedure TfrmMain.edSearchIDExit(Sender: TObject);
begin
  edSearchID.Visible := False;
  edSearchID.Text := '';
end;

procedure TfrmMain.edSearchIDKeyPress(Sender: TObject; var Key: char);
var
  enteredText: String;
  tileID: Integer;
  tileType: Char;
  item: PVirtualItem;
  tileInfo: PTileInfo;
begin
  if Key = #13 then
  begin
    Key := #0;
    enteredText := edSearchID.Text;
    tileType := #0;
    if Length(enteredText) > 1 then
      tileType := enteredText[Length(enteredText)];

    if not (tileType in ['S', 'T']) then
    begin
      if cbTerrain.Checked then
        tileType := 'T'
      else
        tileType := 'S';
    end else
      Delete(enteredText, Length(enteredText), 1);
    
    tileID := 0;
    if not TryStrToInt(enteredText, tileID) then
    begin
      MessageDlg(lbDlgSearchIdErrCaption, lbDlgSearchIdErr, mtError, [mbOK], 0);
      vdtTiles.SetFocus;
      Exit;
    end;
    
    if tileType = 'S' then
      Inc(tileID, $4000);
      
    item := vdtTiles.GetFirst;
    while item <> nil do
    begin
      tileInfo := vdtTiles.GetNodeData(item);
      if tileInfo^.ID = tileID then
      begin
        vdtTiles.ClearSelection;
        vdtTiles.Selected[item] := True;
        vdtTiles.FocusedNode := item;
        Break;
      end;
      item := vdtTiles.GetNext(item);
    end;
    
    if item = nil then
    begin
      MessageDlg(lbDlgNotFoundErrCaption, lbDlgNotFoundErr, mtError, [mbOK], 0);
      vdtTiles.SetFocus;
      Exit;
    end;
    edSearchID.Visible := False;
  end else if Key = #27 then
  begin
    edSearchID.Visible := False;
    Key := #0;
  end else if not (Key in ['$', '0'..'9', 'a'..'f', 'A'..'F', 's', 'S', 't', 'T', #8])
    then Key := #0;
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var UndoRedoManager: TUndoRedoManager;
begin
       if Sender=tvGroups then UndoRedoManager:=FGroupsSelectionUndoRedoManager
  else if Sender=vdtTiles then UndoRedoManager:=FTilesSelectionUndoRedoManager;

  if Button = mbExtra1 then UndoRedoManager.Undo;
  if Button = mbExtra2 then UndoRedoManager.Redo;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var UndoRedoManager: TUndoRedoManager;
begin
       if Sender=tvGroups then UndoRedoManager:=FGroupsSelectionUndoRedoManager
  else if Sender=vdtTiles then UndoRedoManager:=FTilesSelectionUndoRedoManager;

  if Key = VK_OEM_PLUS   then UndoRedoManager.Redo;
  if Key = VK_OEM_MINUS  then UndoRedoManager.Undo;
  if Key = VK_SPACE      then if frmFilter.Visible then begin
                           frmFilter.Locked := True;
                           frmFilter.Hide;
                           frmFilter.Locked := False;
                         end else frmFilter.Show;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  //XMLPropStorage1.Restore;
  XMLPropStorage1RestoreProperties(nil);
  mnuAutoHideGroupListClick(nil);
  mnuAutoHideRandomListClick(nil);
end;

procedure TfrmMain.FormWindowStateChange(Sender: TObject);
//var
//  ws: TWindowState;
//  h: HWND;
begin
  // �диотизм тот еще, но иначе приложение не коректно сворачивается
  // Logger.Send([lcClient, lcDebug], 'Main Window State Chancged');
  if frmMain.WindowState = wsMinimized then begin
    if frmRadarMap.Visible          then frmRadarMap.WindowState          := wsMinimized;
    //if frmRegionControl.Visible     then frmRegionControl.WindowState     := wsMinimized;
    //if frmLargeScaleCommand.Visible then frmLargeScaleCommand.WindowState := wsMinimized;
    //if frmAccountControl.Visible    then frmAccountControl.WindowState    := wsMinimized;
    //if frmAbout.Visible             then frmAbout.WindowState             := wsMinimized;
    Application.Minimize;
  end; // else Application.Restore;

  {h:=frmMain.Handle;
  if frmMain.WindowState = wsMinimized then begin
    ShowWindow(h,SW_HIDE);
    SetWindowLong(h, GWL_EXSTYLE, GetWindowLong(h, GWL_EXSTYLE) and not WS_EX_OVERLAPPEDWINDOW or WS_EX_STATICEDGE);
    ShowWindow(h,SW_SHOWMINIMIZED);
  end else begin
    //ShowInTaskBar := stDefault;
    ShowWindow(h,SW_HIDE);
    SetWindowLong(h, GWL_EXSTYLE, GetWindowLong(h, GWL_EXSTYLE) and not WS_EX_STATICEDGE or WS_EX_OVERLAPPEDWINDOW);
    //ShowInTaskBar := stDefault;
    ShowWindow(h,SW_RESTORE);
  end;}

end;

procedure TfrmMain.lblChatHeaderCaptionClick(Sender: TObject);
begin
  if pnlChat.Visible then
  begin
    pnlChat.Visible := False;
    spChat.Visible := False;
    pnlChatHeader.AnchorSide[akBottom].Control := pcLeft;
    pnlChatHeader.AnchorSide[akBottom].Side := asrBottom;
  end else
  begin
    spChat.Visible := True;
    pnlChat.Visible := True;
    spChat.Top := pnlChat.Top - spChat.Height;
    pnlChatHeader.AnchorSide[akBottom].Control := spChat;
    pnlChatHeader.AnchorSide[akBottom].Side := asrTop;
    
    lblChatHeaderCaption.Font.Bold := False;
    lblChatHeaderCaption.Font.Italic := False;
    lblChatHeaderCaption.Font.Color := clWindowText;
    
    edChat.SetFocus;
  end;
end;

procedure TfrmMain.lblChatHeaderCaptionMouseEnter(Sender: TObject);
begin
  lblChatHeaderCaption.Font.Underline := True;
end;

procedure TfrmMain.lblChatHeaderCaptionMouseLeave(Sender: TObject);
begin
  lblChatHeaderCaption.Font.Underline := False;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.mnuAccountControlClick(Sender: TObject);
begin
  frmAccountControl.Show;
  frmAccountControl.BringToFront;
end;

procedure TfrmMain.mnuTileListViewClick(Sender: TObject);
var
  ViewMode: Word;
  MainTile: Boolean;
  AddHeight: Boolean;

  function UpdateProps(VTList: TVirtualList; ViewMode: Word; AddHeight: Boolean; Update: Boolean): Boolean;
  var
    ColumIdx: Word;
  begin
    Result := False;
    if ((not Update) and (Word(VTList.Tag) = ViewMode))
      then Exit;

    for ColumIdx := 0 to 11 do begin
      VTList.Header.Columns[ColumIdx].Tag := PtrInt(-1);
      VTList.Header.Columns[ColumIdx].Options := VTList.Header.Columns[ColumIdx].Options  - [coVisible];
    end;

    case ViewMode of
      4: ColumIdx := 44;
      3: ColumIdx := 80;
      2: ColumIdx := 96;
      1: ColumIdx := 44;
    end;
    if (AddHeight)
       then inc(ColumIdx, 14);
    VTList.DefaultNodeHeight  := ColumIdx;
    VTList.Tag  := PtrInt(ViewMode);
    if (ViewMode = 1) then begin
      VTList.Header.Options  := VTList.Header.Options  + [hoVisible];
      VTList.TreeOptions.SelectionOptions  := VTList.TreeOptions.SelectionOptions  + [toFullRowSelect];
      VTList.TreeOptions.MiscOptions  := VTList.TreeOptions.MiscOptions  + [toFullRowDrag];
      VTList.Header.Columns[3].Options  := VTList.Header.Columns[3].Options  - [coVisible];
      VTList.Header.Columns[13].Options  := VTList.Header.Columns[13].Options  - [coVisible];
    end else begin
      VTList.Header.Options  := VTList.Header.Options  - [hoVisible];
      VTList.TreeOptions.SelectionOptions  := VTList.TreeOptions.SelectionOptions  - [toFullRowSelect];
      VTList.TreeOptions.MiscOptions  := VTList.TreeOptions.MiscOptions  - [toFullRowDrag];
      VTList.Header.Columns[3].Options  := VTList.Header.Columns[3].Options  + [coVisible];
      VTList.Header.Columns[13].Options  := VTList.Header.Columns[13].Options  + [coVisible];
    end;

    case ViewMode of
      4: begin ColumIdx :=  4; ViewMode :=  7; end;
      3: begin ColumIdx :=  8; ViewMode := 10; end;
      2: begin ColumIdx := 11; ViewMode := 12; end;
      1: begin ColumIdx :=  0; ViewMode :=  2; end;
    end;
    for ColumIdx := ColumIdx to ViewMode do begin
      if (ViewMode <> 2)
        then VTList.Header.Columns[ColumIdx].Tag := PtrInt(Word(VTList.Tag) + ColumIdx - ViewMode - 1)
        else VTList.Header.Columns[ColumIdx].Tag := PtrInt(0);
      VTList.Header.Columns[ColumIdx].Options  := VTList.Header.Columns[ColumIdx].Options  + [coVisible];
    end;

    VTList.BeginUpdate;
    VTList.UpdateTileColumn(Word(VTList.Tag), Update);
    VTList.EndUpdate;
    Result := True;
  end;

begin
  if (Sender <> nil) then begin
    mnuTileListDrawInfo.Enabled := (Sender <> mnuTileListTable);
    mnuMiscTileListDrawInfo.Enabled := (Sender <> mnuMiscTileListTable);
  end;
  MainTile := (Sender = mnuTileListTable) or (Sender = mnuTileListSmall)
           or (Sender = mnuTileListMidle) or (Sender = mnuTileListLarge);
  if ((Sender = mnuTileListTable) or (Sender = mnuMiscTileListTable)) then ViewMode := 1 else
  if ((Sender = mnuTileListSmall) or (Sender = mnuMiscTileListSmall)) then ViewMode := 4 else
  if ((Sender = mnuTileListMidle) or (Sender = mnuMiscTileListMidle)) then ViewMode := 3 else
  if ((Sender = mnuTileListLarge) or (Sender = mnuMiscTileListLarge)) then ViewMode := 2;

  if ((MainTile) or (Sender = nil)) then begin
    if ((Sender = nil) and (mnuTileListTable.Checked)) then ViewMode := 1 else
    if ((Sender = nil) and (mnuTileListSmall.Checked)) then ViewMode := 4 else
    if ((Sender = nil) and (mnuTileListMidle.Checked)) then ViewMode := 3 else
    if ((Sender = nil) and (mnuTileListLarge.Checked)) then ViewMode := 2;
    AddHeight := mnuTileListDrawInfo.Enabled and mnuTileListDrawInfo.Checked;
    UpdateProps(vdtTiles, ViewMode, AddHeight, Sender = nil);
  end;
  if ((not MainTile) or (Sender = nil)) then begin
    if ((Sender = nil) and (mnuMiscTileListTable.Checked)) then ViewMode := 1 else
    if ((Sender = nil) and (mnuMiscTileListSmall.Checked)) then ViewMode := 4 else
    if ((Sender = nil) and (mnuMiscTileListMidle.Checked)) then ViewMode := 3 else
    if ((Sender = nil) and (mnuMiscTileListLarge.Checked)) then ViewMode := 2;
    AddHeight := mnuMiscTileListDrawInfo.Enabled and mnuMiscTileListDrawInfo.Checked;
    UpdateProps(vdlRandom, ViewMode, AddHeight, Sender = nil);
    UpdateProps(frmFilter.vdtFilter, ViewMode, AddHeight, Sender = nil);
    UpdateProps(frmLargeScaleCommand.vdlTerrainTiles, ViewMode, AddHeight, Sender = nil);
    UpdateProps(frmLargeScaleCommand.vdlInsertStaticsTiles, ViewMode, AddHeight, Sender = nil);
    UpdateProps(frmLargeScaleCommand.vdlDeleteStaticsTiles, ViewMode, AddHeight, Sender = nil);
  end;

//     vdtTiles.Clear;
//     BuildTileList;
end;

procedure TfrmMain.mnuTileListDrawClick(Sender: TObject);
var
  dheight: Integer;

  procedure UpdateHeight(TVList: TVirtualList; height: integer);
  var
    TVTree: TVirtualDrawTree;
    node: PVirtualNode;
  begin
    TVTree := TVirtualDrawTree(TVList);
    TVList.BeginUpdate;
    TVList.DefaultNodeHeight := TVList.DefaultNodeHeight + height;
    node := TVTree.GetFirst();
    while (node <> nil) do begin
      node^.NodeHeight := node^.NodeHeight + height;
      node := TVTree.GetNext(node);
    end;
    TVList.EndUpdate;
  end;

begin
  if (Sender = mnuTileListDrawInfo) then begin
    if (mnuTileListDrawInfo.Checked)
      then UpdateHeight(vdtTiles, +14)
      else UpdateHeight(vdtTiles, -14);

  end else if (Sender = mnuMiscTileListDrawInfo) then begin
    if (mnuMiscTileListDrawInfo.Checked)
      then dheight := +14
      else dheight := -14;
    UpdateHeight(vdlRandom, dheight);
    UpdateHeight(frmFilter.vdtFilter, dheight);
    UpdateHeight(frmLargeScaleCommand.vdlTerrainTiles, dheight);
    UpdateHeight(frmLargeScaleCommand.vdlInsertStaticsTiles, dheight);
    UpdateHeight(frmLargeScaleCommand.vdlDeleteStaticsTiles, dheight);
  end;
  // Код является частью отрисовки OnDrawNode => ее надо принудительно вызвать
  vdtTiles.Repaint;
  vdlRandom.Repaint;
  frmFilter.vdtFilter.Repaint;
end;

procedure TfrmMain.mnuAutoHideGroupListClick(Sender: TObject);
begin
  if mnuAutoHideGroupList.Checked then
  begin
    spGroupList.Enabled := (not cbTerrain.Checked) and (not cbStatics.Checked);
    tvGroups.Visible := spGroupList.Enabled;
    if not spGroupList.Enabled
      then mnuAutoHideGroupList.Tag := spGroupList.Top;
      //else mnuAutoHideGroupList.Tag := 0;
  end else
  begin
    if mnuAutoHideGroupList.Tag > 0
      then spGroupList.Top := mnuAutoHideGroupList.Tag;
    mnuAutoHideGroupList.Tag := 0;
    spGroupList.Enabled := True;
    tvGroups.Visible := True;
  end;

  if spGroupList.Enabled then
  begin
    spGroupList.Cursor := crVSplit;
    //if spGroupList.Tag > 0 then
    //  spGroupList.Height := spGroupList.Tag;
    spGroupList.Height := 5;
  end else
  begin
    spGroupList.Cursor := crDefault;
    //spGroupList.Tag := spGroupList.Height;
    spGroupList.Height := 1;
  end;

  spGroupListMoved(Sender);
end;

procedure TfrmMain.mnuAutoHideRandomListClick(Sender: TObject);
begin
  if mnuAutoHideRandomList.Checked then
  begin
    if (not frmDrawSettings.rbRandom.Checked) and (Sender <> nil)
      then mnuAutoHideRandomList.Tag := spTileList.Top;
      //else mnuAutoHideRandomList.Tag := 0;
    spTileList.Enabled := frmDrawSettings.rbRandom.Checked;
    //gbRandom.Visible := frmDrawSettings.rbRandom.Checked;
    cbRandomPreset.Visible := frmDrawSettings.rbRandom.Checked;
    btnRandomPresetSave.Visible := frmDrawSettings.rbRandom.Checked;
    btnRandomPresetDelete.Visible := frmDrawSettings.rbRandom.Checked;
    vdlRandom.Visible := frmDrawSettings.rbRandom.Checked;
  end else
  begin
    if mnuAutoHideRandomList.Tag > 0
      then spTileList.Top := mnuAutoHideRandomList.Tag;
    mnuAutoHideRandomList.Tag := 0;
    spTileList.Enabled := True;
    //gbRandom.Visible := True;
    cbRandomPreset.Visible := True;
    btnRandomPresetSave.Visible := True;
    btnRandomPresetDelete.Visible := True;
    vdlRandom.Visible := True;
  end;

  if spTileList.Enabled then
  begin
    spTileList.Cursor := crVSplit;
    //if spTileList.Tag > 0 then
    //  spTileList.Height := spTileList.Tag;
    spTileList.Height := 5;
  end else
  begin
    spTileList.Cursor := crDefault;
    //spTileList.Tag := spTileList.Height;
    spTileList.Height := 1;
  end;

  spTileListMoved(Sender);
end;

procedure TfrmMain.spGroupListMoved(Sender: TObject);
var
  anchor: integer;
begin
  if mnuAutoHideGroupList.Checked then
    if (spGroupList.Enabled) and (mnuAutoHideGroupList.Tag > 0) then
      begin
        spGroupList.Top := mnuAutoHideGroupList.Tag;
        mnuAutoHideGroupList.Tag := 0;
      end
    else if not spGroupList.Enabled then
      begin
        spGroupList.Top := tvGroups.Top + 4;
        Exit;
      end;

  if spGroupList.Enabled then
    if spGroupList.Top <= tvGroups.Top then
      spGroupList.Top := tvGroups.Top
    else begin
      anchor := spTileList.Top - spGroupList.Height - vdtTiles.Constraints.MinHeight;
      if spGroupList.Top > anchor then spGroupList.Top := (anchor - 1);
    end;
end;

procedure TfrmMain.spTileListMoved(Sender: TObject);
var
  anchor: integer;
begin
  if mnuAutoHideRandomList.Checked then
    if (spTileList.Enabled) and (mnuAutoHideRandomList.Tag > 0) then
      begin
        spTileList.Top := mnuAutoHideRandomList.Tag;
        mnuAutoHideRandomList.Tag := 0;
      end
    else if not spTileList.Enabled then
      begin
        spTileList.Top := gbRandom.Top + gbRandom.Height - spTileList.Tag + 3;
        Exit;
      end;

  if spTileList.Enabled then
    if spTileList.Top + spTileList.Height >= gbRandom.Top + gbRandom.Height then
      spTileList.Top := gbRandom.Top + gbRandom.Height - spTileList.Height
    else begin
      anchor := spGroupList.Top + spGroupList.Height + vdtTiles.Constraints.MinHeight;
      if spTileList.Top < anchor then spTileList.Top := (anchor + 1);
    end;
end;

procedure TfrmMain.tbFilterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If Button = mbRight then
    tbFilter.Tag := PtrInt(GetTickCount);
end;

procedure TfrmMain.tbFilterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if acFilter.Checked and (not frmFilter.Visible) and ((GetTickCount - DWORD(tbFilter.Tag)) < 1000) then begin
    tbFilter.Tag := PtrInt(False);
    frmFilter.Show;
  end;
end;

procedure TfrmMain.pcLeftResize(Sender: TObject);
begin
  spGroupListMoved(Sender);
  spTileListMoved(Sender);
end;

procedure TfrmMain.mnuDisconnectClick(Sender: TObject);
begin
  dmNetwork.Disconnect;
end;

procedure TfrmMain.mnuDocsClick(Sender: TObject);
begin
  //TODO ShellExecute(Handle, 'open', PChar('http://dev.uoquint.ru/projects/centred/wiki'), nil, nil, 1 {SW_SHOWNORMAL});
end;

procedure TfrmMain.mnuRusComClick(Sender: TObject);
begin
  //TODO ShellExecute(Handle, 'open', PChar('http://forum.uokit.com/index.php?showforum=207'), nil, nil, 1 {SW_SHOWNORMAL});
end;

procedure TfrmMain.mnuEng2ComClick(Sender: TObject);
begin
  //TODO ShellExecute(Handle, 'open', PChar('http://craftuo.com/threads/centred.888'), nil, nil, 1 {SW_SHOWNORMAL});
end;

procedure TfrmMain.mnuEngComClick(Sender: TObject);
begin
  //TODO ShellExecute(Handle, 'open', PChar('http://board.uoquint.ru/'), nil, nil, 1 {SW_SHOWNORMAL});
end;

procedure TfrmMain.oglGameWindowPaint(Sender: TObject);
begin
  if mnuWhiteBackground.Checked then
    glClearColor(1, 1, 1, 1)
  else
    glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT);

  InitRender;
  InitSize;

  glDisable(GL_DEPTH_TEST);
  Render;

  oglGameWindow.SwapBuffers;
end;

procedure TfrmMain.oglGameWindowResize(Sender: TObject);
begin
  InvalidateScreenBuffer;
end;

procedure TfrmMain.pbRadarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var posx, posy: Integer;
begin
  posx := self.X+4 * Trunc((Y+X - (pbRadar.Height+pbRadar.Width)div 2)*cos(45));
  posy := self.Y+4 * Trunc((Y-X - (pbRadar.Height-pbRadar.Width)div 2)*cos(45));
  //if (posx < 0) then posx := 0 else if (posx >= 8*Landscape.Width)  then posx := 8*Landscape.Width-1;
  //if (posy < 0) then posy := 0 else if (posy >= 8*Landscape.Height) then posy := 8*Landscape.Height-1;
  if (posx>=0)and(posy>=0)and(posx<8*Landscape.Width)and(posy<8*Landscape.Height)
    then SetPos(posx, posy);
end;

procedure TfrmMain.pbRadarPaint(Sender: TObject);
var
  posX, posY, scrW, scrH: Integer;
  zoom: Single;
  image: TSingleImage;
begin
  posX := (X div 8) - (pbRadar.Width  div 4);
  posY := (Y div 8) - (pbRadar.Height div 4);

  // NOTE: Не очень-то хорошо каждый раз создавать Image...
  image := TSingleImage.CreateFromParams(pbRadar.Width+84, pbRadar.Height+84, ifA8R8G8B8);
  StretchRect(frmRadarMap.Radar.ImageDataPointer^, posX, posY, pbRadar.Width div 2, pbRadar.Height div 2,
           image.ImageDataPointer^, 0, 0, image.Width, image.Height, rfBicubic);
  RotateImage(image.ImageDataPointer^, -45.0);

  DisplayImage(pbRadar.Canvas, (pbRadar.Width-image.Width) div 2, (pbRadar.Height-image.Height) div 2, image);
  image.Free;

  posX := pbRadar.Width  div 2;
  posY := pbRadar.Width  div 2;
  if tbZoom.Down then zoom := tbZoom.Tag / 1000.0 else zoom := 1.0;
  scrW := Trunc(0.25 * oglGameWindow.Width / 44.0 / zoom);
  scrH := Trunc(0.25 * oglGameWindow.Height/ 44.0 / zoom);

  pbRadar.Canvas.Pen.Color := clBlack;
  pbRadar.Canvas.Pen.Style := psSolid;
  pbRadar.Canvas.Line(posX-scrW, posY-scrH, posX+scrW, posY-scrH);
  pbRadar.Canvas.Line(posX+scrW, posY-scrH, posX+scrW, posY+scrH);
  pbRadar.Canvas.Line(posX+scrW, posY+scrH, posX-scrW, posY+scrH);
  pbRadar.Canvas.Line(posX-scrW, posY+scrH, posX-scrW, posY-scrH);

  pbRadar.Canvas.Pen.Color := clSilver;
  pbRadar.Canvas.Pen.Style := psSolid;
  pbRadar.Canvas.Line(posX-scrW-1, posY-scrH-1, posX+scrW+1, posY-scrH-1);
  pbRadar.Canvas.Line(posX+scrW+1, posY-scrH-1, posX+scrW+1, posY+scrH+1);
  pbRadar.Canvas.Line(posX+scrW+1, posY+scrH+1, posX-scrW-1, posY+scrH+1);
  pbRadar.Canvas.Line(posX-scrW-1, posY+scrH+1, posX-scrW-1, posY-scrH-1);
end;

procedure TfrmMain.pmGrabTileInfoPopup(Sender: TObject);
var
  isStatic: Boolean;
begin
  isStatic := CurrentTile is TStaticItem;
  mnuGrabHue.Enabled := isStatic;
  mnuGrabFilterTileID.Enabled := isStatic;
  mnuGrabFilterHue.Enabled := isStatic;
end;

procedure TfrmMain.tbRadarMapClick(Sender: TObject);
begin
  frmRadarMap.Show;
  frmRadarMap.BringToFront;
end;

procedure TfrmMain.tmGrabTileInfoTimer(Sender: TObject);
begin
  tmGrabTileInfo.Enabled := False;
  if CurrentTile <> nil then
    pmGrabTileInfo.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    
  SelectedTile := nil;
end;

procedure TfrmMain.tmMovementTimer(Sender: TObject);
begin
  case FOverlayUI.ActiveArrow of
    0: MoveBy(-1,  0);
    1: MoveBy(-1, -1);
    2: MoveBy( 0, -1);
    3: MoveBy(+1, -1);
    4: MoveBy(+1,  0);
    5: MoveBy(+1, +1);
    6: MoveBy( 0, +1);
    7: MoveBy(-1, +1);
  end;
end;

procedure TfrmMain.tmSettingsCloseTimer(Sender: TObject);
begin
  tmSettingsClose.Enabled := False;
  tbTerrain.Down := acTerrain.Checked;
  tbStatics.Down := acStatics.Checked;
  tbNoDraw.Down  := acNoDraw.Checked;
  tbFlat.Down    := acFlat.Checked;
end;

procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  if Sender is TWinControl then
    if not (TWinControl(Sender)).Focused
      then (TWinControl(Sender)).SetFocus;
end;

procedure TfrmMain.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  nodeData : PGroupNode;
begin
  nodeData := tvGroups.GetNodeData(Node);
  CellText := nodeData^.Name;
end;

procedure TfrmMain.tvGroupsDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
var
  nodeData : PGroupNode;
  TextWidth: Integer;
begin
  nodeData := tvGroups.GetNodeData(Node);

  TargetCanvas.Font.Color := nodeData^.Color;
  TargetCanvas.Font.Bold  := nodeData^.Bold;
  TargetCanvas.Font.Italic:= nodeData^.Ital;
  TargetCanvas.TextOut(CellRect.Left, CellRect.Top+2, CellText);

  TextWidth := TargetCanvas.TextWidth(CellText);
  TargetCanvas.Font.Color := TColor($00BBBBBB);
  TargetCanvas.Font.Bold  := False;
  TargetCanvas.Font.Italic:= False;
  TargetCanvas.TextOut(CellRect.Left + TextWidth + 6, CellRect.Top + 2,
                       Format('(%u)', [nodeData^.Items]));

  DefaultDraw := False;
end;

procedure TfrmMain.tmSelectNodeTimer(Sender: TObject);
begin
  tmSelectNode.Enabled := False;
  if (FGroupsSelectionUndoRedoCommandGroup.UndoRedoCommands.Count > 0) then begin
    if not FGroupsSelectionUndoRedoManager.Enabled then begin
      tmSelectNode.Enabled := True;
      Exit;
    end;
    FGroupsSelectionUndoRedoManager.ExecCommand(FGroupsSelectionUndoRedoCommandGroup);
    FGroupsSelectionUndoRedoCommandGroup := TUndoRedoCommandGroup.Create;
  end;
  //if (FTilesSelectionUndoRedoCommandGroup.UndoRedoCommands.Count > 0) then
    begin
    if not FTilesSelectionUndoRedoManager.Enabled then begin
      tmSelectNode.Enabled := True;
      Exit;
    end;
    FTilesSelectionUndoRedoManager.ExecCommand(FTilesSelectionUndoRedoCommandGroup);
    FTilesSelectionUndoRedoCommandGroup := TUndoRedoCommandGroup.Create;
  end;
end;

procedure TfrmMain.vdtTilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if (Node <> nil) and (FTilesSelectionUndoRedoManager.Enabled) then begin
    FTilesSelectionUndoRedoCommandGroup.Add(
      TUndoRedoSelectVirtualNodeCommand.Create(
        FTilesSelectionUndoRedoManager, Sender, Node)
    );
    if not FTilesSelectionUndoRedoManager.Enabled
      then vdtTiles.FocusedNode := PVirtualItem(Node);
    if not tmSelectNode.Enabled
      then tmSelectNode.Enabled := True
      else tmSelectNode.Interval:= 250;
  end;
end;

procedure TfrmMain.tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  item: PVirtualNode;
begin
  if (Node <> nil) and (FGroupsSelectionUndoRedoManager.Enabled) then begin
    FGroupsSelectionUndoRedoCommandGroup.Add(
      TUndoRedoSelectVirtualNodeCommand.Create(
        FGroupsSelectionUndoRedoManager, Sender, Node)
    );
    if not tmSelectNode.Enabled
      then tmSelectNode.Enabled := True
      else tmSelectNode.Interval:= 250;

    if ((cbStatics.Checked or cbTerrain.Checked) and Sender.Selected[Node]) then begin
      cbStatics.Checked := False;
      cbTerrain.Checked := False;
      Exit;
    end;
  end;

  BuildTileList;
end;

procedure TfrmMain.tvGroupFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  groupData: PGroupNode;
begin
  groupData := Sender.GetNodeData(Node);
  groupData^.Name := EmptyStr;
  FreeMem(groupData^.GLink);
  FreeMem(groupData^.GTile);
  FreeMem(groupData^.Brush);
  FreeMem(groupData^.Entry);
end;

procedure TfrmMain.DropedownMenusClose(Sender: TObject);
begin
  tmSettingsClose.Enabled := True;
end;

procedure TfrmMain.vdtRandomClick(Sender: TObject);
var
  item: PVirtualItem;
  node: PVirtualItem;
  treeNode: PVirtualNode;
  tileInfo: PTileInfo;
  selectedID: Integer;

  function TileInNode(Node: PVirtualNode; TileID: LongWord) : Boolean;
  var
    nodeData: ^TGroupNode;
    i: Integer;
  begin
    Result := False;
    nodeData := tvGroups.GetNodeData(Node);
    for i := 0 to nodeData^.Count - 1 do
    begin
      if nodeData^.GTile[i].ID = TileID then
      begin
        Result := True;
        break;
      end;
    end;
  end;

begin
  if vdlRandom.SelectedCount = 1 then
  begin
    node := vdlRandom.GetFirstSelected;
    if node <> nil then
    begin
      tileInfo := vdlRandom.GetNodeData(node);
      selectedID := tileInfo^.ID;

      // Выбираем группы
      if (not cbStatics.Checked) and (not cbTerrain.Checked) then
      begin
        treeNode := tvGroups.GetFirst();
        while treeNode <> nil do
        begin
          if TileInNode(treeNode, selectedID) then
          begin
            tvGroups.Selected[treeNode] := True;
            tvGroups.FocusedNode := treeNode;
            if toMultiSelect in tvGroups.TreeOptions.SelectionOptions
              then Break;
          end;
            treeNode := tvGroups.GetNext(treeNode);
        end;
      end;

      // Выбираем тайл
      item := vdtTiles.GetFirst;
      while item <> nil do
      begin
        tileInfo := vdtTiles.GetNodeData(item);
        if tileInfo^.ID = selectedID then
        begin
          vdtTiles.ClearSelection;
          vdtTiles.Selected[item] := True;
          vdtTiles.FocusedNode := item;
          item := nil;
        end else
          item := vdtTiles.GetNext(item);
      end;

    end;
  end;
end;

procedure TfrmMain.vdtRandomDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
  if Source = vdtTiles then
    btnAddRandomClick(Sender);
end;

procedure TfrmMain.vdtRandomDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
begin
  if source = vdtTiles then Accept := True;
end;

procedure TfrmMain.vdtRandomLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  tileInfo: PTileInfo;
begin
  tileInfo := Sender.GetNodeData(Node);
  Stream.Read(tileInfo^.ID, SizeOf(tileInfo^.ID));
end;

procedure TfrmMain.vdtRandomSaveNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  tileInfo: PTileInfo;
begin
  tileInfo := Sender.GetNodeData(Node);
  Stream.Write(tileInfo^.ID, SizeOf(tileInfo^.ID));
end;

procedure TfrmMain.vdtRandomUpdating(Sender: TBaseVirtualTree;
  State: TVTUpdateState);
begin
  if acDraw.Checked then
    ProcessToolState;
end;

procedure TfrmMain.vdtTilesClick(Sender: TObject);
begin
  if acDraw.Checked then
    ProcessToolState;
end;

procedure TfrmMain.vdtTilesDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  //vdtTiles.auto
  Allowed := True;
end;

procedure TfrmMain.vdtTilesDrawHint(Sender: TBaseVirtualTree;
  HintCanvas: TCanvas; Node: PVirtualNode; const R: TRect; Column: TColumnIndex
  );
var
  m_name : string;
begin
  vdtTiles.UpdateHintCanvas(HintCanvas);

  HintCanvas.Font.Assign(Sender.Font);
  HintCanvas.Font.Style := [fsBold];
  m_name := CP1251ToUTF8(FTileHint.Name);
  DrawText(HintCanvas.Handle, PChar(m_name), Length(m_name), FTileHint.NameRect, 0);
  HintCanvas.Font.Style := [];
  DrawText(HintCanvas.Handle, PChar(FTileHint.Obj), Length(FTileHint.Obj), FTileHint.ObjRect, 0);
  HintCanvas.Font.Style := [fsItalic];
  DrawText(HintCanvas.Handle, PChar(FTileHint.Flags), Length(FTileHint.Flags),
    FTileHint.FlagsRect, DT_WORDBREAK);
end;

procedure TfrmMain.vdtTilesDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var
  tileInfo: PTileInfo;
  textStyle: TTextStyle;
  artEntry: TArt;
  tileData: TTileData;
  id: LongWord;
  rect, CellRect:TRect;
  image: TSingleImage;
  colorOld, colorNew: Word;
  DrawInfo: Boolean;
  oldcolor: DWORD;
  newcolor: DWORD;
  srcCanvas, dstCanvas: TFastARGB32Canvas;
  tmpImage : TSingleImage;
  bit:  Integer;

  procedure DisplayNodeImage(main: Boolean; const Canvas: TCanvas; const CellRect: TRect; Image: TBaseImage; ForceStretch: Boolean = False);
  var
    Resised: Float;
    DstSize: TSize;
    DstRect: TRect;
    //SrcRect: TRect;
  begin
    if (main and mnuTileListStretch.Checked) or (not main and mnuMiscTileListStretch.Checked) or (ForceStretch) then begin

      if ForceStretch or (DstSize.cy <> 44) then begin
        DstSize.cx := CellRect.Right - CellRect.Left;
        DstSize.cy := CellRect.Bottom - CellRect.Top;
        if ForceStretch then begin
          if (DstSize.cx <= 44) then id := 2
          else if (DstSize.cx <= 80) then id := 3
          else id := 4;
          DstSize.cx := DstSize.cx - id;
          DstSize.cy := DstSize.cy - id;
        end;
        Resised := Min(Min(Float(DstSize.cx)/Float(Image.Width), Float(DstSize.cy)/Float(Image.Height)), 1.0);
        DstSize.cx := Trunc(Resised * Float(Image.Width));
        DstSize.cy := Trunc(Resised * Float(Image.Height));
        DstRect.Left := Trunc((CellRect.Left + CellRect.Right - DstSize.cx) / 2);
        DstRect.Right := DstRect.Left + DstSize.cx;
        if not ForceStretch
          then DstRect.Bottom := CellRect.Bottom
          else DstRect.Bottom := (CellRect.Bottom - CellRect.Top + DstSize.cy) div 2;
        DstRect.Top := DstRect.Bottom - DstSize.cy
      end else begin
        DstRect := CellRect;
      end;
      DisplayImage(Canvas, DstRect, Image);
    end else
    if (main and mnuTileListClip.Checked) or (not main and mnuMiscTileListClip.Checked) then begin
      //DstSize.cx := CellRect.Right - CellRect.Left;
      //DstSize.cy := CellRect.Bottom - CellRect.Top;
      //DstRect := CellRect;
      //SrcRect.Top := 0;
      //if (DstSize.cy <= Image.Height) then begin
      //  SrcRect.Bottom := DstSize.cy;
      //end else begin
      //  SrcRect.Bottom := Image.Height;
      //  DstRect.Bottom := DstRect.Top + Image.Height;
      //end;
      //if (DstSize.cx <= Image.Width) then begin
      //  SrcRect.Left := (Image.Width - DstSize.cx) div 2;
      //  SrcRect.Right:= SrcRect.Left + DstSize.cx;
      //end else begin
      //  SrcRect.Left := 0;
      //  SrcRect.Right:= Image.Width;
      //  DstRect.Left := DstRect.Left + (DstSize.cx - Image.Width) div 2;
      //  DstRect.Right:= DstRect.Left + Image.Width;
      //end;
      //Logger.Send([lcClient, lcDebug], 'TfrmMain.vdtTilesDrawNode [%d,%d,%d,%d](%d,%d) - [%d,%d,%d,%d](%d,%d) I(%d,%d) C[%d,%d,%d,%d](%d,%d)',
      //[SrcRect.Top, SrcRect.Left, SrcRect.Bottom, SrcRect.Right, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
      // DstRect.Top, DstRect.Left, DstRect.Bottom, DstRect.Right, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      // Image.Width, Image.Height,
      // CellRect.Top, CellRect.Left, CellRect.Bottom, CellRect.Right, CellRect.Right - CellRect.Left, CellRect.Bottom - CellRect.Top]);

      //DisplayImage(Canvas, DstRect, Image, SrcRect);
      DisplayImage(Canvas, Trunc((CellRect.Left + CellRect.Right - Image.Width) / 2), CellRect.Top, Image);
      Canvas.FillRect(CellRect.Left, CellRect.Bottom, CellRect.Right, CellRect.Bottom + 14);
    end else
    if (main and mnuTileListCentre.Checked) or (not main and mnuMiscTileListCentre.Checked) then begin
      // TODO: ....
    end;
  end;

begin
  if (not (Sender is TVirtualList))
    then tileInfo := Sender.GetNodeData(PaintInfo.Node)
    else tileInfo := vdtTiles.GetNodeData(PVirtualItem(PaintInfo.Node));

  //if (sender <> vdtTiles) then begin
  //Logger.Send([lcClient, lcDebug], 'TfrmMain.vdtTilesDrawNode %.8x at cell [%d,%d]', [tileInfo^.ID, PaintInfo.Column, PaintInfo.Node^.Index]);
  //Exit;
  //end;
//  Logger.Send([lcClient, lcDebug], 'TfrmMain.vdtTilesDrawNode %d', [tileInfo^.ID]);

  if (tileInfo^.ID = $FFFFFFFF) then exit;

  textStyle := PaintInfo.Canvas.TextStyle;
  textStyle.Alignment := taCenter;
  textStyle.Layout := tlCenter;
  textStyle.Wordbreak := True;
  PaintInfo.Canvas.Font.Color := Sender.Font.Color;
  if (Sender = vdtTiles) then
    PaintInfo.Canvas.Font.Color := TColor($808080);

  //Logger.Send([lcClient, lcDebug], 'vdtTilesDrawNode %.4x', [tileInfo^.ID]);

  case PaintInfo.Column of
    0:
      begin
        id := tileInfo^.ID;
        if id > $2F000000 then
          PaintInfo.Canvas.TextRect(PaintInfo.CellRect, 0, 0, Format('BL%.3d', [id - $2F000000]), textStyle)
        else if id > $1F000000 then
          PaintInfo.Canvas.TextRect(PaintInfo.CellRect, 0, 0, Format('ET%.3d', [id - $1F000000]), textStyle)
        else begin
        if id > $00003FFF then Dec(id, $00004000);
        //PaintInfo.Canvas.TextRect(PaintInfo.CellRect, 0, 0, '4x', textStyle);
        PaintInfo.Canvas.TextRect(PaintInfo.CellRect, 0, 0, Format('$%.4x', [id]), textStyle);
        end;
      end;
    2:
      begin
        textStyle.Alignment := taLeftJustify;
        if (tileInfo^.ID > $2F000000) then begin          // Кисти
          PaintInfo.Canvas.TextRect(PaintInfo.CellRect, PaintInfo.CellRect.Left + 4,
            PaintInfo.CellRect.Top, PGroupBrush(tileInfo^.ptr)^.Name, textStyle);
        end else if (tileInfo^.ID > $1F000000) then begin // Объекты
          PaintInfo.Canvas.TextRect(PaintInfo.CellRect, PaintInfo.CellRect.Left + 4,
            PaintInfo.CellRect.Top, PGroupEntry(tileInfo^.ptr)^.Name, textStyle);
        end else begin  // Тайлы
          tileData := TTileData(ResMan.Tiledata.Block[tileInfo^.ID]);
          PaintInfo.Canvas.TextRect(PaintInfo.CellRect, PaintInfo.CellRect.Left + 4,
            PaintInfo.CellRect.Top, CP1251ToUTF8(Trim(tileData.TileName)),
            textStyle);
          tileData.Free;
        end;
      end;
    3:  begin Exit; end;
    13: begin Exit; end;
    // 1: - 3: 4: 5: 6: - 7: 8: 9: - 10: 11:
    else
      begin
        CellRect := PaintInfo.CellRect;
        if (PaintInfo.Column = 1)
          then DrawInfo := False
        else if (sender = vdtTiles)
          then DrawInfo := mnuTileListDrawInfo.Enabled and mnuTileListDrawInfo.Checked
          else DrawInfo := mnuMiscTileListDrawInfo.Enabled and mnuMiscTileListDrawInfo.Checked;
        if DrawInfo then begin
          CellRect.Bottom := CellRect.Bottom - 14;
        end;

        if (tileInfo^.ID > $2F000000) then begin         // Кисти
          //image := TSingleImage.CreateFromImage(PGroupBrush(tileInfo^.ptr)^.Image);
          colorOld := $0000; colorNew := RGB2ARGB(PaintInfo.Canvas.Pixels[CellRect.Left,CellRect.Top]);
          PGroupBrush(tileInfo^.ptr)^.Image.ReplaceColor(0,0,PGroupBrush(tileInfo^.ptr)^.Image.Width,PGroupBrush(tileInfo^.ptr)^.Image.Height,
          @colorOld, @colorNew);
          image := TSingleImage.CreateFromImage(PGroupBrush(tileInfo^.ptr)^.Image);

          image.Format := ifDefault;
          DisplayNodeImage((Sender = vdtTiles), PaintInfo.Canvas, CellRect, image, True);
          //DisplayImage(PaintInfo.Canvas, CellRect, image);
          image.Free;
        end else if (tileInfo^.ID > $1F000000) then begin// Объекты
          //colorOld := $0000; colorNew := RGB2ARGB($00FFFFFF);
          //PGroupEntry(tileInfo^.ptr)^.Image.ReplaceColor(0,0,PGroupEntry(tileInfo^.ptr)^.Image.Width,PGroupEntry(tileInfo^.ptr)^.Image.Height,
          //@colorOld, @colorNew);
          image := TSingleImage.CreateFromImage(PGroupEntry(tileInfo^.ptr)^.Image);
          //oldcolor := PDWORD(Image.Bits)^;
          //newcolor := $00FF0000;//PDWORD(Image.)^;
          //image.ReplaceColor(0,0,Image.Width,Image.Height,@oldcolor,@newcolor);
          //image.Format := ifDefault;
          DisplayNodeImage((Sender = vdtTiles), PaintInfo.Canvas, CellRect, image, True);
          image.Free;
        end else if ResMan.Art.Exists(tileInfo^.ID) then // Тайлы
        begin
          artEntry := ResMan.Art.GetArt(tileInfo^.ID,
            RGB2ARGB(PaintInfo.Canvas.Pixels[CellRect.Left, CellRect.Top]), nil, False);
          DisplayNodeImage((Sender = vdtTiles), PaintInfo.Canvas, CellRect, artEntry.Graphic);
          artEntry.Free;
          if tileInfo^.ID > $3FFF then
            if (FLightSourceTiles[tileInfo^.ID - $4000].image > 0) then // Источники света
            begin
              bit := FLightSourceTiles[tileInfo^.ID - $4000].image;
              //rect.Right:=CellRect.Right;
              //rect.Bottom:=CellRect.Bottom;
              //rect.Left:=CellRect.Right - 24;
              //rect.Top:=CellRect.Bottom - 24;
              //DisplayImage(PaintInfo.Canvas, rect, FVLightSrcImage);

              //DisplayNodeImage((Sender = vdtTiles), PaintInfo.Canvas, CellRect, FVLightSrcImage, True);

              rect.Left := 0;
              rect.Top  := 0;
              rect.Right:= FVLightSrcImage[bit].Width;
              rect.Bottom:=FVLightSrcImage[bit].Height;
              tmpImage := TSingleImage.CreateFromParams(rect.Right, rect.Bottom, ifA8R8G8B8);
              srcCanvas := TFastARGB32Canvas.CreateForImage(FVLightSrcImage[bit]);
              dstCanvas := TFastARGB32Canvas.CreateForImage(tmpImage);

              dstCanvas.FillColor32 := PaintInfo.Canvas.Pixels[CellRect.Left+4,CellRect.Top+4];
              dstCanvas.FillRect(rect);
              srcCanvas.DrawAlpha(rect, dstCanvas, 0,0);
              srcCanvas.Free;
              dstCanvas.Free;
              DisplayNodeImage((Sender = vdtTiles), PaintInfo.Canvas, CellRect, tmpImage, True);
              tmpImage.Free;
            end;
        end;

        if DrawInfo then begin // Подписи тайлов
          CellRect.Bottom := PaintInfo.CellRect.Bottom;
          rect := PaintInfo.CellRect;
          rect.Top := rect.Bottom - 14;;
          id := tileInfo^.ID;
          if id > $2F000000 then
            PaintInfo.Canvas.TextRect(rect, 0, 0, Format('BL%.3d', [id - $2F000000]), textStyle)
          else if id > $1F000000 then
            PaintInfo.Canvas.TextRect(rect, 0, 0, Format('ET%.3d', [id - $1F000000]), textStyle)
          else begin
            if id > $00003FFF then Dec(id, $00004000);
            //PaintInfo.Canvas.TextRect(rect, 0, 0, '4x', textStyle);
            PaintInfo.Canvas.TextRect(rect, 0, 0, Format('0x%.4x', [id]), textStyle);
          end;
        end;
      end;

  end;
end;

procedure TfrmMain.vdtTilesEnter(Sender: TObject);
begin
  if acFilter.Checked and mnuAutoShowFilterWindow.Checked and (not frmFilter.Visible) and (not frmFilter.Locked) then
  begin
    frmFilter.Locked := True;
    frmFilter.Show;
    frmMain.SetFocus;
    frmFilter.Locked := False;
  end;
end;

procedure TfrmMain.vdtTilesGetHintSize(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var R: TRect);
var
  tileInfo: PTileInfo;
  tileData: TTiledata;
  prefix, flags: string;
  id: LongWord;
  cmHint: TCMHintShow;

  procedure UpdateFlags(AFlag: TTileDataFlag; AName: string);
  begin
    if AFlag in tileData.Flags then
    begin
      if flags <> '' then
        flags := flags + ', ' + AName
      else
        flags := AName;
    end;
  end;

begin
  case Word(Sender.Tag) of
    1: id := 0;
    2: id := min(max(0, Column - 10), 2);
    3: id := min(max(0, Column -  7), 3);
    4: id := min(max(0, Column -  3), 4);
  end;
  //Logger.Send([lcClient, lcDebug], 'vdtTilesGetHintSize', Column);
  tileInfo := (TVirtualList(Sender)).GetNodeData(vdtTiles.GetItemAt(Node, Column));// @PTileInfo(Sender.GetNodeData(Node))[id];
  flags := '';
//  if (FTileHint.Column = id) then Exit;
//  FTileHint.Column := id;

  if (tileInfo^.ID > $2F000000) then begin
    FTileHint.Name := UTF8ToCP1251(PGroupBrush(tileInfo^.ptr)^.Name);
    FTileHint.Obj := Format('Brush ID: 0x%.4x (%.5d)', [tileInfo^.ID-$2F000000, tileInfo^.ID-$2F000000]);
    //flags := 'Brush';
  end else if (tileInfo^.ID > $1F000000) then begin
    FTileHint.Name := UTF8ToCP1251(PGroupEntry(tileInfo^.ptr)^.Name);
    FTileHint.Obj := Format('Entry ID: 0x%.4x (%.5d)', [tileInfo^.ID-$1F000000, tileInfo^.ID-$1F000000]);
    //flags := 'Entry';
  end else
  begin
    tileData := ResMan.Tiledata.TileData[tileInfo^.ID];
    if tileInfo^.ID < $4000 then begin
      if TLandTiledata(tileData).TextureID > 0 then
        flags := 'Stretchable';
      FTileHint.Obj := Format('LandT ID: 0x%.4x (%.5d)', [tileInfo^.ID, tileInfo^.ID]);
    end else begin
      FTileHint.Obj := Format('ItemT ID: 0x%.4x (%.5d)', [tileInfo^.ID-$4000, tileInfo^.ID-$4000]);
    end;


    if tdfArticleA in tileData.Flags then
      prefix := 'a '
    else if tdfArticleAn in tileData.Flags then
      prefix := 'an '
    else
      prefix := '';

    FTileHint.Name := AnsiProperCase(Format('%s%s', [prefix, tileData.TileName]), [' ']);

    UpdateFlags(tdfBackground, 'Background');
    UpdateFlags(tdfWeapon, 'Weapon');
    UpdateFlags(tdfTransparent, 'Transparent');
    UpdateFlags(tdfTranslucent, 'Translucent');
    UpdateFlags(tdfWall, 'Wall');
    UpdateFlags(tdfDamaging, 'Damaging');
    UpdateFlags(tdfImpassable, 'Impassable');
    UpdateFlags(tdfWet, 'Wet');
    UpdateFlags(tdfSurface, 'Surface');
    UpdateFlags(tdfBridge, 'Bridge');
    UpdateFlags(tdfGeneric, 'Generic');
    UpdateFlags(tdfWindow, 'Window');
    UpdateFlags(tdfNoShoot, 'NoShoot');
    UpdateFlags(tdfInternal, 'Internal');
    UpdateFlags(tdfFoliage, 'Foliage');
    UpdateFlags(tdfPartialHue, 'PartialHue');
    UpdateFlags(tdfMap, 'Map');
    UpdateFlags(tdfContainer, 'Container');
    UpdateFlags(tdfWearable, 'Wearable');
    UpdateFlags(tdfLightSource, 'Lightsource');
    UpdateFlags(tdfAnimation, 'Animation');
    UpdateFlags(tdfNoDiagonal, 'NoDiagonal');  //HoverOver
    UpdateFlags(tdfArmor, 'Armor');
    UpdateFlags(tdfRoof, 'Roof');
    UpdateFlags(tdfDoor, 'Door');
    UpdateFlags(tdfStairBack, 'StairBack');
    UpdateFlags(tdfStairRight, 'StairRight');
  end;

  FTileHint.NameRect.Left := 5;
  FTileHint.NameRect.Top := 5;
  Sender.Canvas.Font.Style := [fsBold];
  DrawText(Sender.Canvas.Handle, PChar(CP1251ToUTF8(FTileHint.Name)), Length(CP1251ToUTF8(FTileHint.Name)),
    FTileHint.NameRect, DT_CALCRECT);
  FTileHint.ObjRect.Left := 5;
  FTileHint.ObjRect.Top := FTileHint.NameRect.Bottom + 6;
  Sender.Canvas.Font.Style := [];
  DrawText(Sender.Canvas.Handle, PChar(FTileHint.Obj), Length(FTileHint.Obj), FTileHint.ObjRect, DT_CALCRECT);
  FTileHint.Flags := Format('Flags = [%s]', [flags]);
  FTileHint.FlagsRect.Left := 5;
  FTileHint.FlagsRect.Top := FTileHint.ObjRect.Bottom + 2;
  FTileHint.FlagsRect.Right := 145;
  Sender.Canvas.Font.Style := [fsItalic];
  DrawText(Sender.Canvas.Handle, PChar(FTileHint.Flags), Length(FTileHint.Flags),
    FTileHint.FlagsRect, DT_CALCRECT or DT_WORDBREAK);

  R := Rect(0, 0, Max(Max(FTileHint.NameRect.Right, FTileHint.ObjRect.Right), FTileHint.FlagsRect.Right) + 5,
    FTileHint.FlagsRect.Bottom + 5);
end;

procedure TfrmMain.vdtTilesKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['$', '0'..'9'] then
  begin
    edSearchID.Text := Key;
    edSearchID.Visible := True;
    edSearchID.SetFocus;
    edSearchID.SelStart := 1;
    Key := #0;
  end;
end;

procedure TfrmMain.vdtTilesScroll(Sender: TBaseVirtualTree; DeltaX,
  DeltaY: Integer);
begin
  if Sender.CanFocus and Sender.MouseEntered then
    Sender.SetFocus;
end;

procedure TfrmMain.vstChatClick(Sender: TObject);
begin
  edChat.SetFocus;
end;

procedure TfrmMain.vstChatFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  chatInfo: PChatInfo;
begin
  chatInfo := Sender.GetNodeData(Node);
  chatInfo^.Sender := '';
  chatInfo^.Msg := '';
end;

procedure TfrmMain.vstChatGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  chatInfo: PChatInfo;
begin
  chatInfo := Sender.GetNodeData(Node);
  case Column of
    0: CellText := TimeToStr(chatInfo^.Time);
    1: CellText := chatInfo^.Sender;
    2: CellText := chatInfo^.Msg;
  end;
end;

procedure TfrmMain.vstChatPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  chatInfo: PChatInfo;
begin
  chatInfo := Sender.GetNodeData(Node);
  if chatInfo^.Sender = 'System' then
  begin
    if Column = 1 then
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic, fsBold]
    else
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsItalic];
  end;
end;

procedure TfrmMain.vstClientsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  clientInfo: PClientInfo;
begin
  clientInfo := Sender.GetNodeData(Node);
  case Column of
    1: CellText := clientInfo^.Name;//Format('%d, %d', [locationInfo^.X, locationInfo^.Y]);
    2: CellText := FormatDateTime('mm\dd - hh:mm:ss', clientInfo^.LogonDateTime);
  end;
end;

procedure TfrmMain.vstClientsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  clientInfo: PClientInfo;
begin
  clientInfo := Sender.GetNodeData(Node);
  case Column of
    0: frmAccountControl.GetAccountImageIndex(clientInfo^.AccessLevel, ImageIndex);
  end;
end;

procedure TfrmMain.vstLocationsDblClick(Sender: TObject);
var
  node: PVirtualNode;
  locationInfo: PLocationInfo;
begin
  node := vstLocations.GetFirstSelected;
  if node <> nil then
  begin
    locationInfo := vstLocations.GetNodeData(node);
    SetPos(locationInfo^.X, locationInfo^.Y);
  end;
end;

procedure TfrmMain.vstLocationsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  locationInfo: PLocationInfo;
begin
  locationInfo := Sender.GetNodeData(Node);
  locationInfo^.Name := EmptyStr;
end;

procedure TfrmMain.vstLocationsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  locationInfo: PLocationInfo;
begin
  locationInfo := Sender.GetNodeData(Node);
  case Column of
    0: CellText := Format('%d, %d', [locationInfo^.X, locationInfo^.Y]);
    1: CellText := locationInfo^.Name;
  end;
end;

procedure TfrmMain.vstLocationsLoadNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  locationInfo: PLocationInfo;
  stringLength: Integer;
  s: string;
begin
  locationInfo := Sender.GetNodeData(Node);
  Stream.Read(locationInfo^.X, SizeOf(Word));
  Stream.Read(locationInfo^.Y, SizeOf(Word));
  stringLength := 0;
  Stream.Read(stringLength, SizeOf(Integer));
  SetLength(s, stringLength);
  Stream.Read(s[1], stringLength);
  locationInfo^.Name := s;
end;

procedure TfrmMain.vstLocationsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  locationInfo: PLocationInfo;
begin
  if Column = 1 then
  begin
    locationInfo := Sender.GetNodeData(Node);
    locationInfo^.Name := NewText;
  end;
end;

procedure TfrmMain.vstLocationsSaveNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  locationInfo: PLocationInfo;
  stringLength: Integer;
begin
  locationInfo := Sender.GetNodeData(Node);
  Stream.Write(locationInfo^.X, SizeOf(Word));
  Stream.Write(locationInfo^.Y, SizeOf(Word));
  stringLength := Length(locationInfo^.Name);
  Stream.Write(stringLength, SizeOf(Integer));
  Stream.Write(locationInfo^.Name[1], stringLength);
end;

procedure TfrmMain.XMLPropStorage1RestoreProperties(Sender: TObject);
var ws: Integer;
begin
  Logger.EnterMethod([lcLandscape, lcDebug], 'TfrmMain.XMLPropStorage1RestoreProperties(Sender: TObject);');

  if Sender = nil then begin
    FTextureManager.UseAnims := mnuShowAnimations.Checked;
    tbTerrain.Down := acTerrain.Checked;
    frmLightLevel.tbLightlevel.Position:=acLightLevel.Tag;
  end;

  if mnuWindowedMode.Checked then begin
    BorderStyle := bsSizeable;
    FormStyle   := fsNormal;
    {if ((Tag<>0) and (mnuWindowedMode.Tag<>0)) then begin
      ws := (Tag and $3);
      if ws = 1 then WindowState := wsNormal     else
      if ws = 2 then WindowState := wsMinimized  else
      if ws = 3 then WindowState := wsMaximized;
      Left  := ((mnuWindowedMode.Tag  shr 15) and $7FFF);
      Top   := (mnuWindowedMode.Tag and $7FFF);
      Width := ((Tag  shr 16) and $3FFF);
      Height:= ((Tag  shr  2) and $3FFF);
      Tag := 0; mnuWindowedMode.Tag := 0;
    end; }
  end else begin
    {if ((Tag = 0) and (mnuWindowedMode.Tag = 0)) then begin
      if WindowState = wsNormal    then ws := 1  else
      if WindowState = wsMinimized then ws := 2  else
      if WindowState = wsMaximized then ws := 3  else ws := 0;
      Tag := ((Width and $3FFF) shl 16) or ((Height and $3FFF) shl 2) or (ws and $3);
      mnuWindowedMode.Tag := ((Left and $7FFF) shl 15) or ((Top and $7FFF) shl 0);
    end; }
    BorderStyle := bsNone;
    FormStyle   := fsStayOnTop;
    WindowState := wsMaximized;
  end;

  //mnuTileListViewClick(nil);
  //mnuAutoHideGroupListClick(nil);
  //mnuAutoHideRandomListClick(nil);
  Logger.ExitMethod([lcLandscape, lcDebug], 'TfrmMain.XMLPropStorage1RestoreProperties(Sender: TObject);');
end;

procedure TfrmMain.XMLPropStorage1SavingProperties(Sender: TObject);
var ws: Integer;
begin
  Logger.EnterMethod([lcLandscape, lcDebug], 'TfrmMain.XMLPropStorage1SavingProperties(Sender: TObject);');
  //Hide;
  if mnuWindowedMode.Checked then begin
    if ((Tag<>0) and (mnuWindowedMode.Tag<>0)) then begin
      ws := (Tag and $3);
      if ws = 1 then WindowState := wsNormal     else
      if ws = 2 then WindowState := wsMinimized  else
      if ws = 3 then WindowState := wsMaximized;
      Left  := ((mnuWindowedMode.Tag  shr 15) and $7FFF);
      Top   := (mnuWindowedMode.Tag and $7FFF);
      Width := ((Tag  shr 16) and $3FFF);
      Height:= ((Tag  shr  2) and $3FFF);
      Tag := 0; mnuWindowedMode.Tag := 0;
    end;
  end else begin
    if ((Tag = 0) and (mnuWindowedMode.Tag = 0)) then begin
      if WindowState = wsNormal    then ws := 1  else
      if WindowState = wsMinimized then ws := 2  else
      if WindowState = wsMaximized then ws := 3  else ws := 0;
      Tag := ((Width and $3FFF) shl 16) or ((Height and $3FFF) shl 2) or (ws and $3);
      mnuWindowedMode.Tag := ((Left and $7FFF) shl 15) or ((Top and $7FFF) shl 0);
    end;
  end;
  //Show;
  if spTileList.Top > spTileList.Parent.Height then
    spTileList.Top := spTileList.Parent.Height - 200;
  Logger.ExitMethod([lcLandscape, lcDebug], 'TfrmMain.XMLPropStorage1SavingProperties(Sender: TObject);');
end;

procedure TfrmMain.SetX(const AValue: Integer);
begin
  SetPos(AValue, FY);
end;

procedure TfrmMain.SetY(const AValue: Integer);
begin
  SetPos(FX, AValue);
end;

procedure TfrmMain.SetPos(AX, AY: Word);
begin
  if InRange(AX, 0, FLandscape.CellWidth - 1) and InRange(AY, 0,
    FLandscape.CellHeight - 1) then
  begin
    FX := AX;
    edX.Value := FX;
    FY := AY;
    edY.Value := FY;
    dmNetwork.Send(TUpdateClientPosPacket.Create(AX, AY));
    InvalidateScreenBuffer;
    if frmRadarMap <> nil then frmRadarMap.Repaint;
    pbRadar.Repaint;
  end;
end;

procedure TfrmMain.SwitchToSelection;
begin
  acSelect.Checked := True;
  BringToFront;
end;

procedure TfrmMain.RegisterAccessChangedListener(
  AListener: TAccessChangedListener);
begin
  if FAccessChangedListeners.IndexOf(AListener) < 0 then
    FAccessChangedListeners.Add(AListener);
end;

procedure TfrmMain.RegisterSelectionListener(AListener: TSelectionListener);
begin
  if FSelectionListeners.IndexOf(AListener) < 0 then
    FSelectionListeners.Add(AListener);
end;

procedure TfrmMain.UnregisterAccessChangedListener(
  AListener: TAccessChangedListener);
begin
  FAccessChangedListeners.Remove(AListener);
end;

procedure TfrmMain.UnregisterSelectionListener(AListener: TSelectionListener);
begin
  FSelectionListeners.Remove(AListener);
end;

procedure TfrmMain.SetCurrentTile(const AValue: TWorldItem);
var
  sRect: TRect;
  w, h: Integer;
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'SetCurrentTile');
  if AValue = FCurrentTile then
  begin
    //Logger.ExitMethod([lcClient, lcDebug], 'SetCurrentTile');
    Exit;
  end;
  //Logger.Send([lcClient, lcDebug], 'Value', AValue);

  if FCurrentTile <> nil then
    FCurrentTile.OnDestroy.UnregisterEvent(@OnTileRemoved);
  FCurrentTile := AValue;

  if FCurrentTile = nil then
  begin
    lblTileInfoIDLabel.Caption:= '';
    lblTileInfoIDValue.Caption:= '';
    lblTileInfoXLabel.Caption := '';
    lblTileInfoXValue.Caption := '';
    lblTileInfoYLabel.Caption := '';
    lblTileInfoYValue.Caption := '';
    lblTileInfoZLabel.Caption := '';
    lblTileInfoZValue.Caption := '';
    lblTileInfoHueLabel.Caption := '';
    lblTileInfoHueValue.Caption := '';
    lblTileInfoOLabel.Visible := False;
    lblTileInfoWLabel.Visible := False;
    lblTileInfoWValue.Visible := False;
    lblTileInfoHLabel.Visible := False;
    lblTileInfoHValue.Visible := False;
    lblTileInfoCLabel.Visible := False;
  end else
  begin
    FCurrentTile.OnDestroy.RegisterEvent(@OnTileRemoved);
    lblTileInfoIDValue.Caption := Format('0x%.4x', [FCurrentTile.TileID]);
    lblTileInfoIDValue.Font.Bold := True;
    lblTileInfoXLabel.Caption := lbBottomCursorPosX;
    lblTileInfoYLabel.Caption := lbBottomCursorPosY;
    lblTileInfoZLabel.Caption := lbBottomCursorPosZ;
    lblTileInfoXValue.Caption  := Format('%d', [FCurrentTile.X]);
    lblTileInfoYValue.Caption  := Format('%d', [FCurrentTile.Y]);
    lblTileInfoZValue.Caption  := Format('%d', [FCurrentTile.Z]);
    if FCurrentTile is TVirtualTile then begin
      lblTileInfoIDLabel.Caption := lbBottomCursorVLayer1;
      lblTileInfoIDValue.Font.Bold := False;
      lblTileInfoIDValue.Caption := lbBottomCursorVLayer2;
      lblTileInfoHueLabel.Caption:= '';
      lblTileInfoHueValue.Caption:= '';
    end
    else if FCurrentTile is TMapCell then begin
      lblTileInfoIDLabel.Caption := lbBottomCursorLandID;
      lblTileInfoHueLabel.Caption:= '';
      lblTileInfoHueValue.Caption:= '';
    end
    else if FCurrentTile is TStaticItem then begin
      lblTileInfoIDLabel.Caption := lbBottomCursorItemID;
      lblTileInfoHueLabel.Caption:= lbBottomCursorItemHue;
      lblTileInfoHueValue.Caption:= Format('%.3x', [TStaticItem(FCurrentTile).Hue]);
    end;
    sRect := GetSelectedRect;
    w := sRect.Right - sRect.Left + 1;
    h := sRect.Bottom - sRect.Top + 1;
    if ((w > 1) or (h > 1)) then begin
      lblTileInfoOLabel.Visible := True;
      lblTileInfoWLabel.Visible := True;
      lblTileInfoWValue.Visible := True;
      lblTileInfoHLabel.Visible := True;
      lblTileInfoHValue.Visible := True;
      lblTileInfoCLabel.Visible := True;
      lblTileInfoWValue.Caption := Format('%d', [w]);
      lblTileInfoHValue.Caption := Format('%d', [h]);
    end else begin
      lblTileInfoOLabel.Visible := False;
      lblTileInfoWLabel.Visible := False;
      lblTileInfoWValue.Visible := False;
      lblTileInfoHLabel.Visible := False;
      lblTileInfoHValue.Visible := False;
      lblTileInfoCLabel.Visible := False;
    end;
  end;

  UpdateSelection;

  //Logger.Send([lcClient, lcDebug], 'CurrentTile: %.5x (%.6d)', [CurrentTile.TileID, CurrentTile.TileID]);
  //Logger.ExitMethod([lcClient, lcDebug], 'SetCurrentTile');
end;

procedure TfrmMain.SetSelectedTile(const AValue: TWorldItem);
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'SetSelectedTile');
  if AValue = FSelectedTile then
  begin
    Logger.ExitMethod([lcClient, lcDebug], 'SetSelectedTile');
    Exit;
  end;
  //Logger.Send([lcClient, lcDebug], 'Value', AValue);

  if FSelectedTile <> nil then
    FSelectedTile.OnDestroy.UnregisterEvent(@OnTileRemoved);
  FSelectedTile := AValue;
  if FSelectedTile <> nil then
    FSelectedTile.OnDestroy.RegisterEvent(@OnTileRemoved);

  UpdateSelection;
  //Logger.ExitMethod([lcClient, lcDebug], 'SetSelectedTile');
end;

procedure TfrmMain.SetNormalLights;
const
  specular: TGLArrayf4 = (2, 2, 2, 1);
  ambient: TGLArrayf4 = (1, 1, 1, 1);
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT, @specular);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @ambient);
end;

procedure TfrmMain.SetDarkLights;
const
  specularDark: TGLArrayf4 = (0.5, 0.5, 0.5, 1);
  ambientDark: TGLArrayf4 = (0.25, 0.25, 0.25, 1);
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT, @specularDark);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @ambientDark);
end;

procedure TfrmMain.InitRender;
const
  lightPosition: TGLArrayf4 = (-1, -1, 0.5, 0);
begin
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 0.1);
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_DITHER);
  glEnable(GL_BLEND); // Enable alpha blending of textures
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_NORMALIZE);

  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_POSITION, @lightPosition);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
end;

procedure TfrmMain.InitSize;
begin
  glViewport(0, 0, oglGameWindow.Width, oglGameWindow.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0, oglGameWindow.Width, oglGameWindow.Height, 0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TfrmMain.LoadLocations;
var
  xmlDoc: TXMLDocument;
  location: TDOMElement;
  locationNode: PVirtualNode;
  locationInfo: PLocationInfo;
  locations: TDOMNodeList;
  i, j: Integer;
begin
  vstLocations.Clear;

  if FileExists(FLocationsFile) then
  begin
    ReadXMLFile(xmlDoc, FLocationsFile);
    if xmlDoc.DocumentElement.NodeName = 'Locations' then
    begin
      locations := xmlDoc.DocumentElement.ChildNodes;
      for i := 0 to locations.Count - 1 do
      begin
        location := TDOMElement(locations[i]);
        if location.NodeName = 'Location' then
        begin
          locationNode := vstLocations.AddChild(nil);
          locationInfo := vstLocations.GetNodeData(locationNode);
          locationInfo^.Name := CP1251ToUTF8(location.AttribStrings['Name']);

          if TryStrToInt(location.AttribStrings['X'], j) then
            locationInfo^.X := j
          else
            locationInfo^.X := 0;

          if TryStrToInt(location.AttribStrings['Y'], j) then
            locationInfo^.Y := j
          else
            locationInfo^.Y := 0;
        end;
      end;
    end;

    xmlDoc.Free;
  end;
end;

procedure TfrmMain.LoadRandomPresets;
var
  presets: TDOMNodeList;
  i: Integer;
begin
  cbRandomPreset.Clear;

  FreeAndNil(FRandomPresetsDoc);
  if FileExists(FRandomPresetsFile) then
  begin
    ReadXMLFile(FRandomPresetsDoc, FRandomPresetsFile);
    presets := FRandomPresetsDoc.DocumentElement.ChildNodes;
    for i := 0 to presets.Count - 1 do
    begin
      if presets[i].NodeName = 'Preset' then
      begin
        cbRandomPreset.Items.AddObject(
          CP1251ToUTF8(TDOMElement(presets[i]).AttribStrings['Name']), presets[i]);
      end;
    end;
  end else
  begin
    FRandomPresetsDoc := TXMLDocument.Create;
    FRandomPresetsDoc.AppendChild(FRandomPresetsDoc.CreateElement('RandomPresets'));
  end;
end;

procedure TfrmMain.MoveBy(AOffsetX, AOffsetY: Integer); inline;
begin
  SetPos(EnsureRange(FX + AOffsetX, 0, FLandscape.CellWidth - 1),
         EnsureRange(FY + AOffsetY, 0, FLandscape.CellHeight - 1));
  UpdateCurrentTile;
end;

procedure TfrmMain.PrepareMapCell(AMapCell: TMapCell);
var
  current, north, east, west,   tile1, tile4, tile3, tile6: PBlockInfo;
  cell: TMapCell;
begin
  current := FScreenBuffer.UpdateSortOrder(AMapCell);
  if current = nil then
    Exit; //off-screen update

  PrepareScreenBlock(current);
  Exclude(FScreenBufferState, sbsIndexed);

  //Find surrounding cells
  current := nil;
  north := nil;
  east := nil;
  west := nil;
  tile1 := nil;
  tile4 := nil;
  tile3 := nil;
  tile6 := nil;
  while ((north = nil) or (east = nil) or (west = nil) or
    (tile1 = nil) or (tile3 = nil) or (tile4 = nil) or (tile6 = nil)) and
    FScreenBuffer.Iterate(current) do
  begin
    if current^.Item is TMapCell then
    begin
      cell := TMapCell(current^.Item);
      if (cell.X = AMapCell.X - 1) and (cell.Y = AMapCell.Y - 1) then
        north := current
      else if (cell.X = AMapCell.X) and (cell.Y = AMapCell.Y - 1) then
        east := current
      else if (cell.X = AMapCell.X - 1) and (cell.Y = AMapCell.Y) then
        west := current
      else if (cell.X = AMapCell.X) and (cell.Y = AMapCell.Y + 1) then
        tile1 := current
      else if (cell.X = AMapCell.X + 1) and (cell.Y = AMapCell.Y) then
        tile3 := current
      else if (cell.X = AMapCell.X - 1) and (cell.Y = AMapCell.Y + 1) then
        tile4 := current
      else if (cell.X = AMapCell.X + 1) and (cell.Y = AMapCell.Y - 1) then
        tile6 := current;
    end;
  end;

  if north <> nil then PrepareScreenBlock(north);
  if east <> nil then PrepareScreenBlock(east);
  if west <> nil then PrepareScreenBlock(west);
  if tile1 <> nil then PrepareScreenBlock(tile1);
  if tile3 <> nil then PrepareScreenBlock(tile3);
  if tile4 <> nil then PrepareScreenBlock(tile4);
  if tile6 <> nil then PrepareScreenBlock(tile6);

end;

procedure TfrmMain.InvalidateFilter;
begin
  Exclude(FScreenBufferState, sbsFiltered);
end;

procedure TfrmMain.InvalidateScreenBuffer;
begin
  Exclude(FScreenBufferState, sbsValid);
end;

procedure TfrmMain.PrepareScreenBlock(ABlockInfo: PBlockInfo);

  procedure GetLandAlt(const AX, AY: Integer; const ADefaultZ,
    ADefaultRaw: SmallInt; out Z, RawZ: SmallInt);
  var
    cell: TMapCell;
  begin
    cell := FLandscape.MapCell[AX, AY];
    if cell <> nil then
    begin
      Z := cell.Z;
      RawZ := cell.RawZ;
    end else
    begin
      Z := ADefaultZ;
      RawZ := ADefaultRaw;
    end;
  end;

var
  item: TWorldItem;
  drawX, drawY: Integer;
  z, west, south, east, tileNorth, tileWest, tileLeft, tileRight: SmallInt;
  rawZ, rawWest, rawSouth, rawEast, rawTileNorth, rawTileWest, rawTileLeft, rawTileRight: SmallInt;
  staticItem: TStaticItem;
  zoom: Single;
begin
  //add normals to map tiles and materials where possible

  item := ABlockInfo^.Item;

  GetDrawOffset(item.X , item.Y, drawX, drawY);

  if acFlat.Checked then
  begin
    z := 0;
    rawZ := 0;
  end else
  begin
    z := item.Z;
    rawZ := item.RawZ;
  end;

  if ABlockInfo^.HighRes <> nil then ABlockInfo^.HighRes.DelRef;
  if ABlockInfo^.LowRes <> nil then ABlockInfo^.LowRes.DelRef;

  ABlockInfo^.HighRes := nil;
  ABlockInfo^.CheckRealQuad := False;
  ABlockInfo^.Text.Free;

  if item is TMapCell then
  begin
    if not acFlat.Checked then
    begin
      GetLandAlt(item.X, item.Y + 1, z, rawZ, west, rawWest);
      GetLandAlt(item.X + 1, item.Y + 1, z, rawZ, south, rawSouth);
      GetLandAlt(item.X + 1, item.Y, z, rawZ, east, rawEast);

      if  (west <> z) or (south <> z) or (east <> z) then
        ABlockInfo^.HighRes := FTextureManager.GetTexMaterial(item.TileID);

      if (rawWest <> rawZ) or (rawSouth <> rawZ) or (rawEast <> rawZ) then
      begin
        ABlockInfo^.RealQuad[0][0] := drawX;
        ABlockInfo^.RealQuad[0][1] := drawY - rawZ * 4;
        ABlockInfo^.RealQuad[1][0] := drawX + 22;
        ABlockInfo^.RealQuad[1][1] := drawY + 22 - rawEast * 4;
        ABlockInfo^.RealQuad[2][0] := drawX;
        ABlockInfo^.RealQuad[2][1] := drawY + 44 - rawSouth * 4;
        ABlockInfo^.RealQuad[3][0] := drawX - 22;
        ABlockInfo^.RealQuad[3][1] := drawY + 22 - rawWest * 4;

        with ABlockInfo^ do
        begin
          with ScreenRect do
          begin
            Left := drawX - 22;
            Right := drawX + 22;
            Top := RealQuad[0][1];
            Bottom := RealQuad[0][1];

            if RealQuad[1][1] < Top then Top := RealQuad[1][1];
            if RealQuad[1][1] > Bottom then Bottom := RealQuad[1][1];

            if RealQuad[2][1] < Top then Top := RealQuad[2][1];
            if RealQuad[2][1] > Bottom then Bottom := RealQuad[2][1];

            if RealQuad[3][1] < Top then Top := RealQuad[3][1];
            if RealQuad[3][1] > Bottom then Bottom := RealQuad[3][1];
          end;
          CheckRealQuad := True;
        end;
      end;
    end else
    begin
      if mnuFlatShowHeight.Checked then
        ABlockInfo^.Text := TGLText.Create(FGLFont, IntToStr(item.Z));
    end;

    if not ABlockInfo^.CheckRealQuad then
      ABlockInfo^.ScreenRect := Bounds(Trunc(drawX - 22),
        Trunc(drawY - rawZ * 4), 44, 44);

    ABlockInfo^.LowRes := FTextureManager.GetArtMaterial(item.TileID);

    if ABlockInfo^.HighRes <> nil then
    begin
      if ABlockInfo^.Normals = nil then
        New(ABlockInfo^.Normals);
      FLandscape.GetNormals(item.X, item.Y, ABlockInfo^.Normals^);
      ABlockInfo^.DrawQuad[0][0] := drawX;
      ABlockInfo^.DrawQuad[0][1] := drawY - z * 4;
      ABlockInfo^.DrawQuad[1][0] := drawX + 22;
      ABlockInfo^.DrawQuad[1][1] := drawY + 22 - east * 4;
      ABlockInfo^.DrawQuad[2][0] := drawX;
      ABlockInfo^.DrawQuad[2][1] := drawY + 44 - south * 4;
      ABlockInfo^.DrawQuad[3][0] := drawX - 22;
      ABlockInfo^.DrawQuad[3][1] := drawY + 22 - west * 4;

      // Подготовка сетки рельефа для текстур
      if mnuShowGrid.Checked or mnuShowBlocks.Checked then
      begin
        ABlockInfo^.LineWidth[0] := 0.9;
        ABlockInfo^.LineDraw[0][0] := ABlockInfo^.DrawQuad[0];
        ABlockInfo^.LineDraw[0][1] := ABlockInfo^.DrawQuad[1];
        ABlockInfo^.LineWidth[1] := 0.9;
        ABlockInfo^.LineDraw[1][0] := ABlockInfo^.DrawQuad[0];
        ABlockInfo^.LineDraw[1][1] := ABlockInfo^.DrawQuad[3];
        ABlockInfo^.LineWidth[2] := 0.8;
        //ABlockInfo^.LineDraw[2][0] := ABlockInfo^.DrawQuad[0];
        //ABlockInfo^.LineDraw[2][1] := ABlockInfo^.DrawQuad[2];
        ABlockInfo^.LineDraw[2][0] := ABlockInfo^.DrawQuad[1];
        ABlockInfo^.LineDraw[2][1] := ABlockInfo^.DrawQuad[3];
      end;
    end else
    begin
      ABlockInfo^.DrawQuad[0][0] := drawX - 22;
      ABlockInfo^.DrawQuad[0][1] := drawY - z * 4;
      ABlockInfo^.DrawQuad[1][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[1][1] := drawY - z * 4;
      ABlockInfo^.DrawQuad[2][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[2][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;
      ABlockInfo^.DrawQuad[3][0] := drawX - 22;
      ABlockInfo^.DrawQuad[3][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;

      // Подготовка сетки рельефа для лендов
      if mnuShowGrid.Checked or mnuShowBlocks.Checked then
      begin
        GetLandAlt(item.X, item.Y - 1, z, rawZ, tileNorth, rawTileNorth);
        GetLandAlt(item.X - 1, item.Y, z, rawZ, tileWest, rawTileWest);
        GetLandAlt(item.X - 1, item.Y + 1, z, rawZ, tileLeft, rawTileLeft);
        GetLandAlt(item.X + 1, item.Y - 1, z, rawZ, tileRight, rawTileRight);
        if (tileNorth <> z) or (tileRight <> z)
          then ABlockInfo^.LineWidth[0] := 0.9
          else ABlockInfo^.LineWidth[0] := 0.8;
        ABlockInfo^.LineDraw[0][0][0] := drawX;
        ABlockInfo^.LineDraw[0][0][1] := ABlockInfo^.DrawQuad[0][1];
        ABlockInfo^.LineDraw[0][1][0] := ABlockInfo^.DrawQuad[1][0];
        ABlockInfo^.LineDraw[0][1][1] := ABlockInfo^.DrawQuad[1][1] + 22;
        if (tileWest <> z) or (tileLeft <> z)
          then ABlockInfo^.LineWidth[1] := 0.9
          else ABlockInfo^.LineWidth[1] := 0.8;
        ABlockInfo^.LineDraw[1][0][0] := drawX;
        ABlockInfo^.LineDraw[1][0][1] := ABlockInfo^.DrawQuad[0][1];
        ABlockInfo^.LineDraw[1][1][0] := ABlockInfo^.DrawQuad[3][0];
        ABlockInfo^.LineDraw[1][1][1] := ABlockInfo^.DrawQuad[3][1] - 22;
        ABlockInfo^.LineWidth[2] := 0.0;
      end;
    end;
  end else
  // Виртуальный пол
  if item is TVirtualTile then
  begin
    ABlockInfo^.LowRes := FVLayerMaterial;
    ABlockInfo^.LowRes.AddRef;
    ABlockInfo^.ScreenRect := Bounds(Trunc(drawX - 22), Trunc(drawY - z * 4),
      44, 44);
    ABlockInfo^.DrawQuad[0][0] := drawX - 22;
    ABlockInfo^.DrawQuad[0][1] := drawY - z * 4;
    ABlockInfo^.DrawQuad[1][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
    ABlockInfo^.DrawQuad[1][1] := drawY - z * 4;
    ABlockInfo^.DrawQuad[2][0] := drawX - 22 + ABlockInfo^.LowRes.Width;
    ABlockInfo^.DrawQuad[2][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;
    ABlockInfo^.DrawQuad[3][0] := drawX - 22;
    ABlockInfo^.DrawQuad[3][1] := drawY + ABlockInfo^.LowRes.Height - z * 4;

    // Подготовка сетки блоков на виртуальном полу
    if (frmVirtualLayer.cbShowBlocks.Checked) then
    begin
      if (item.Y mod 8) <> 0 then
        ABlockInfo^.LineWidth[0] := 0
      else begin
        ABlockInfo^.LineWidth[0] := 1.6;
        ABlockInfo^.LineDraw[0][0][0] := drawX;
        ABlockInfo^.LineDraw[0][0][1] := ABlockInfo^.DrawQuad[0][1];
        ABlockInfo^.LineDraw[0][1][0] := ABlockInfo^.DrawQuad[1][0];
        ABlockInfo^.LineDraw[0][1][1] := ABlockInfo^.DrawQuad[1][1] + 22;
      end;
      if (item.X mod 8) <> 0 then
        ABlockInfo^.LineWidth[1] := 0
      else begin
        ABlockInfo^.LineWidth[1] := 1.6;
        ABlockInfo^.LineDraw[1][0][0] := drawX;
        ABlockInfo^.LineDraw[1][0][1] := ABlockInfo^.DrawQuad[0][1];
        ABlockInfo^.LineDraw[1][1][0] := ABlockInfo^.DrawQuad[3][0];
        ABlockInfo^.LineDraw[1][1][1] := ABlockInfo^.DrawQuad[3][1] - 22;
      end;

    end;

  end else
  begin
    staticItem := TStaticItem(item);

    // Тайл виртуального источника света
    if acNoDraw.Checked and mnuShowLightSource.Checked and (staticItem.TileID <= FLandscape.MaxStaticID)
       and (FLightSourceTiles[staticItem.TileID].image > 0) then
    begin
      ABlockInfo^.LowRes := FVLightSrcMaterial[FLightSourceTiles[staticItem.TileID].image - 1];
      ABlockInfo^.LowRes.AddRef;
      ABlockInfo^.ScreenRect := Bounds(Trunc(drawX - 12), Trunc(drawY - z * 4), 24, 24);
      ABlockInfo^.DrawQuad[0][0] := drawX - 12;
      ABlockInfo^.DrawQuad[0][1] := drawY + 12 - z * 4;
      ABlockInfo^.DrawQuad[1][0] := drawX - 12 + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[1][1] := drawY + 12 - z * 4;
      ABlockInfo^.DrawQuad[2][0] := drawX - 12 + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[2][1] := drawY + 12 + ABlockInfo^.LowRes.Height - z * 4;
      ABlockInfo^.DrawQuad[3][0] := drawX - 12;
      ABlockInfo^.DrawQuad[3][1] := drawY + 12 + ABlockInfo^.LowRes.Height - z * 4;
    end else
    // Тайлы обычной статики
    begin
      ABlockInfo^.LowRes := FTextureManager.GetStaticMaterial(staticItem);
      ABlockInfo^.HueOverride := False;

      ABlockInfo^.ScreenRect := Bounds(Trunc(drawX - ABlockInfo^.LowRes.RealWidth / 2),
        Trunc(drawY + 44 - ABlockInfo^.LowRes.RealHeight - z * 4),
        ABlockInfo^.LowRes.RealWidth,
        ABlockInfo^.LowRes.RealHeight);

      ABlockInfo^.Translucent := tdfTranslucent in
        ResMan.Tiledata.StaticTiles[staticItem.TileID].Flags;

      south := ABlockInfo^.LowRes.RealHeight;
      east := ABlockInfo^.LowRes.RealWidth div 2;

      ABlockInfo^.DrawQuad[0][0] := drawX - east;
      ABlockInfo^.DrawQuad[0][1] := drawY + 44 - south - z * 4;
      ABlockInfo^.DrawQuad[1][0] := drawX - east + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[1][1] := drawY + 44 - south - z * 4;
      ABlockInfo^.DrawQuad[2][0] := drawX - east + ABlockInfo^.LowRes.Width;
      ABlockInfo^.DrawQuad[2][1] := drawY + 44 - south + ABlockInfo^.LowRes.Height - z * 4;
      ABlockInfo^.DrawQuad[3][0] := drawX - east;
      ABlockInfo^.DrawQuad[3][1] := drawY + 44 - south + ABlockInfo^.LowRes.Height - z * 4;
    end;
  end;


  if tbZoom.Down then begin
    zoom := tbZoom.Tag / 1000.0;

    ABlockInfo^.ScreenRect.Left   := Trunc(zoom * (ABlockInfo^.ScreenRect.Left   -drawX)+drawX);
    ABlockInfo^.ScreenRect.Top    := Trunc(zoom * (ABlockInfo^.ScreenRect.Top    -drawY)+drawY);;
    ABlockInfo^.ScreenRect.Right  := Trunc(zoom * (ABlockInfo^.ScreenRect.Right  -drawX)+drawX);
    ABlockInfo^.ScreenRect.Bottom := Trunc(zoom * (ABlockInfo^.ScreenRect.Bottom -drawY)+drawY);

    //south:= Trunc(zoom*(ABlockInfo^.DrawQuad[2][1] - ABlockInfo^.DrawQuad[0][1]));
    //east := Trunc(zoom*(ABlockInfo^.DrawQuad[2][0] - ABlockInfo^.DrawQuad[0][0]));

    for z:=0 to 3 do begin
      ABlockInfo^.DrawQuad[z][0] := Trunc(zoom * (ABlockInfo^.DrawQuad[z][0] -drawX)+drawX);
      ABlockInfo^.DrawQuad[z][1] := Trunc(zoom * (ABlockInfo^.DrawQuad[z][1] -drawY)+drawY);
    end;
    if item is TMapCell then begin
      inc(ABlockInfo^.DrawQuad[0][1], -1);
      inc(ABlockInfo^.DrawQuad[1][0], +1);
      inc(ABlockInfo^.DrawQuad[2][1], +1);
      inc(ABlockInfo^.DrawQuad[3][0], -1);
    end else if zoom < 1.0 then begin
      inc(ABlockInfo^.DrawQuad[0][0], -1);
      inc(ABlockInfo^.DrawQuad[0][1], -1);
      //inc(ABlockInfo^.DrawQuad[1][0], +1);
      inc(ABlockInfo^.DrawQuad[1][1], -1);
      //inc(ABlockInfo^.DrawQuad[2][0], +1);
      //inc(ABlockInfo^.DrawQuad[2][1], +1);
      inc(ABlockInfo^.DrawQuad[3][0], -1);
      //inc(ABlockInfo^.DrawQuad[3][1], +1);
    end;

    if not acFlat.Checked and (item is TMapCell) and
       ((rawWest <> rawZ) or (rawSouth <> rawZ) or (rawEast <> rawZ)) then
    for z:=0 to 3 do begin
      ABlockInfo^.RealQuad[z][0] := Trunc(zoom * (ABlockInfo^.RealQuad[z][0] -drawX)+drawX);
      ABlockInfo^.RealQuad[z][1] := Trunc(zoom * (ABlockInfo^.RealQuad[z][1] -drawY)+drawY);
    end;

    rawZ := -1;
    if ((mnuShowGrid.Checked or mnuShowBlocks.Checked) and (item is TMapCell))
      then if (ABlockInfo^.HighRes <> nil) then rawZ:=2 else rawZ:= 1
    else if (frmVirtualLayer.cbShowBlocks.Checked and (item is TVirtualTile))
      then if ((item.Y mod 8 = 0) or (item.X mod 8 = 0)) then rawZ := 1;

    for z:=0 to rawZ do begin
      //ABlockInfo^.LineWidth[z] := zoom * ABlockInfo^.LineWidth[z];
      ABlockInfo^.LineDraw[z][0][0] := TGLint(Trunc(zoom *(ABlockInfo^.LineDraw[z][0][0] -drawX)+drawX));
      ABlockInfo^.LineDraw[z][0][1] := TGLint(Trunc(zoom *(ABlockInfo^.LineDraw[z][0][1] -drawY)+drawY));
      ABlockInfo^.LineDraw[z][1][0] := TGLint(Trunc(zoom *(ABlockInfo^.LineDraw[z][1][0] -drawX)+drawX));
      ABlockInfo^.LineDraw[z][1][1] := TGLint(Trunc(zoom *(ABlockInfo^.LineDraw[z][1][1] -drawY)+drawY));
    end;
  end;
end;

procedure TfrmMain.Render;
var
  highlight: Boolean;
  intensity, red, green, blue: GLfloat;
  blockInfo: PBlockInfo;
  item: TWorldItem;
  i : byte;
  zoom: Single;
begin
  if tbZoom.Down then zoom := tbZoom.Tag / 1000.0 else zoom:=1.0;

  if not (sbsValid in FScreenBufferState) then
    RebuildScreenBuffer;

  if not (sbsIndexed in FScreenBufferState) then
  begin
    FScreenBuffer.UpdateShortcuts;
    Include(FScreenBufferState, sbsIndexed);
  end;

  if not (sbsFiltered in FScreenBufferState) then
    UpdateFilter;

  blockInfo := nil;
  while FScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.State = ssFiltered then
      Continue;

    item := blockInfo^.Item;

    if acSelect.Checked or item.CanBeEdited or (item is TVirtualTile) then
    begin
      intensity := 1.0;
      SetNormalLights;
    end else
    begin
      intensity := 0.5;
      SetDarkLights;
    end;

    case blockInfo^.WalkRestriction of
      wrNone:
        begin
          red := 1;
          green := 1;
          blue := 1;
        end;
      wrCanWalk:
        begin
          red := 0.5;
          green := 1;
          blue := 0.5;
        end;
      wrCannotWalk:
        begin
          red := 1;
          green := 0.5;
          blue := 0.5;
        end;
    end;

    if blockInfo^.Translucent then
      glColor4f(intensity * red, intensity * green, intensity * blue, 0.8)
    else
      glColor4f(intensity * red, intensity * green, intensity * blue, 1.0);

    highlight := blockInfo^.Highlighted and item.CanBeEdited;

    if highlight then
    begin
      glEnable(GL_COLOR_LOGIC_OP);
      glLogicOp(GL_COPY_INVERTED);
    end;

    if blockInfo^.HighRes <> nil then
    begin
      glBindTexture(GL_TEXTURE_2D, blockInfo^.HighRes.Texture);

      if not highlight and (blockInfo^.WalkRestriction = wrNone) then
        glEnable(GL_LIGHTING);

      glBegin(GL_QUADS);
        glNormal3fv(@blockInfo^.Normals^[0]);
        glTexCoord2i(0, 0); glVertex2iv(@blockInfo^.DrawQuad[0]);
        glNormal3fv(@blockInfo^.Normals^[3]);
        glTexCoord2i(0, 1); glVertex2iv(@blockInfo^.DrawQuad[3]);
        glNormal3fv(@blockInfo^.Normals^[2]);
        glTexCoord2i(1, 1); glVertex2iv(@blockInfo^.DrawQuad[2]);
        glNormal3fv(@blockInfo^.Normals^[1]);
        glTexCoord2i(1, 0); glVertex2iv(@blockInfo^.DrawQuad[1]);
      glEnd;

      if not highlight and (blockInfo^.WalkRestriction = wrNone) then
        glDisable(GL_LIGHTING);
    end else
    begin
      glBindTexture(GL_TEXTURE_2D, blockInfo^.LowRes.Texture);
      glBegin(GL_QUADS);
        glTexCoord2i(0, 0); glVertex2iv(@blockInfo^.DrawQuad[0]);
        glTexCoord2i(1, 0); glVertex2iv(@blockInfo^.DrawQuad[1]);
        glTexCoord2i(1, 1); glVertex2iv(@blockInfo^.DrawQuad[2]);
        glTexCoord2i(0, 1); glVertex2iv(@blockInfo^.DrawQuad[3]);
      glEnd;
    end;

    if highlight then
      glDisable(GL_COLOR_LOGIC_OP);

    if (blockInfo^.Text <> nil) then
      blockInfo^.Text.Render(blockInfo^.ScreenRect);

    /////////////////
    // Рендинг сетки рельефа
    if (mnuShowGrid.Checked) and (blockInfo^.Item is TMapCell) then
    begin
      glDisable(GL_TEXTURE_2D);                // не использовать цвет текстуры
      glEnable(GL_LINE_SMOOTH);                // сглаживание линий
      //glDisable(GL_LINE_SMOOTH);

      if (tbFlat.Down) then
      begin
        glColor4f(0.8, 0.8, 0.8, 0.9);         // цвет линий
        glLineWidth(0.1);                      // ширина линий
        glBegin(GL_LINES);
          glVertex2iv(@blockInfo^.LineDraw[0][0]);
          glVertex2iv(@blockInfo^.LineDraw[0][1]);
          glVertex2iv(@blockInfo^.LineDraw[1][0]);
          glVertex2iv(@blockInfo^.LineDraw[1][1]);
        glEnd;
      end else
      begin
        if blockInfo^.LineWidth[0] < 0.85
          then glColor4f(1.0, 1.0, 0.0, 0.5)   // цвет линий
          else glColor4f(1.0, 1.0, 1.0, 1.0);  // цвет линий
        glLineWidth(blockInfo^.LineWidth[0]);  // ширина линий
        glBegin(GL_LINES);
          glVertex2iv(@blockInfo^.LineDraw[0][0]);
          glVertex2iv(@blockInfo^.LineDraw[0][1]);
        glEnd;
        if blockInfo^.LineWidth[1] < 0.85
          then glColor4f(1.0, 1.0, 0.0, 0.5)   // цвет линий
          else glColor4f(1.0, 1.0, 1.0, 1.0);  // цвет линий
        glLineWidth(blockInfo^.LineWidth[1]);  // ширина линий
        glBegin(GL_LINES);
          glVertex2iv(@blockInfo^.LineDraw[1][0]);
          glVertex2iv(@blockInfo^.LineDraw[1][1]);
        glEnd;
        if blockInfo^.LineWidth[2] > 0 then
          begin
            glColor4f(1.3281, 0.2510, 1.0, 0.8); // цвет линий
            glLineWidth(blockInfo^.LineWidth[2]);// ширина линий
            glBegin(GL_LINES);
              glVertex2iv(@blockInfo^.LineDraw[2][0]);
              glVertex2iv(@blockInfo^.LineDraw[2][1]);
            glEnd;
          end;
      end;

      glDisable(GL_LINE_SMOOTH);
      glEnable(GL_TEXTURE_2D);
    end else if (mnuShowBlocks.Checked) and (blockInfo^.Item is TMapCell) then
    begin  // Рендинг сетки блоков
      glDisable(GL_TEXTURE_2D);                // не использовать цвет текстуры
      glEnable(GL_LINE_SMOOTH);                // сглаживание линий

      if (blockInfo^.Item.X mod 8 = 0) then begin
        glLineWidth(2.0);
        glColor4f(1.0, 1.0, 0.0, 0.5);
      end else begin
        glColor4f(0.8, 0.8, 0.8, 0.9);
        glLineWidth(0.1);
      end;
      glBegin(GL_LINES);
        glVertex2iv(@blockInfo^.LineDraw[1][0]);
        glVertex2iv(@blockInfo^.LineDraw[1][1]);
      glEnd;
      if (blockInfo^.Item.Y mod 8 = 0) then begin
        glLineWidth(2.0);
        glColor4f(1.0, 1.0, 0.0, 0.5);
      end else begin
        glColor4f(0.8, 0.8, 0.8, 0.9);
        glLineWidth(0.1);
      end;
      glBegin(GL_LINES);
        glVertex2iv(@blockInfo^.LineDraw[0][0]);
        glVertex2iv(@blockInfo^.LineDraw[0][1]);
      glEnd;

      glDisable(GL_LINE_SMOOTH);
      glEnable(GL_TEXTURE_2D);
    end else if (frmVirtualLayer.cbShowBlocks.Checked) and (blockInfo^.Item is TVirtualTile) then
    begin
      glDisable(GL_TEXTURE_2D);                // не использовать цвет текстуры
      glEnable(GL_LINE_SMOOTH);                // сглаживание линий

      for i := 0 to 1 do if blockInfo^.LineWidth[i] > 0 then begin
        glColor4f(0.0, 0.65, 0.68, 0.50);      // цвет линий
        glLineWidth(blockInfo^.LineWidth[i]);  // ширина линий
        glBegin(GL_LINES);
          glVertex2iv(@blockInfo^.LineDraw[i][0]);
          glVertex2iv(@blockInfo^.LineDraw[i][1]);
        glEnd;
      end;

      glDisable(GL_LINE_SMOOTH);
      glEnable(GL_TEXTURE_2D);
    end;

    ////////////////
  end;

  if (FLightManager.LightLevel > 0) and not acFlat.Checked then
    FLightManager.Draw(oglGameWindow.ClientRect, zoom);
  FOverlayUI.Draw(oglGameWindow);

  FLandscape.ResizeBlockCache(0);
end;

procedure TfrmMain.SaveLocations;
var
  xmlDoc: TXMLDocument;
  location: TDOMElement;
  locationNode: PVirtualNode;
  locationInfo: PLocationInfo;
begin
  xmlDoc := TXMLDocument.Create;
  xmlDoc.AppendChild(xmlDoc.CreateElement('Locations'));

  locationNode := vstLocations.GetFirst;
  while locationNode <> nil do
  begin
    locationInfo := vstLocations.GetNodeData(locationNode);
    location := xmlDoc.CreateElement('Location');
    location.AttribStrings['Name'] := UTF8ToCP1251(locationInfo^.Name);
    location.AttribStrings['X'] := IntToStr(locationInfo^.X);
    location.AttribStrings['Y'] := IntToStr(locationInfo^.Y);
    xmlDoc.DocumentElement.AppendChild(location);

    locationNode := vstLocations.GetNext(locationNode);
  end;

  WriteXMLFile(xmlDoc, FLocationsFile);
  xmlDoc.Free;
end;

procedure TfrmMain.SaveRandomPresets;
begin
  WriteXMLFile(FRandomPresetsDoc, FRandomPresetsFile);
end;

procedure TfrmMain.OnLandscapeChanged;
begin
  InvalidateScreenBuffer;
  oglGameWindow.Repaint;
  UpdateCurrentTile;
end;

procedure TfrmMain.OnMapChanged(AMapCell: TMapCell);
begin
  PrepareMapCell(AMapCell);
  ForceUpdateCurrentTile;
  InvalidateFilter
end;

procedure TfrmMain.OnNewBlock(ABlock: TBlock);
begin
  InvalidateScreenBuffer;
end;

procedure TfrmMain.OnStaticDeleted(AStaticItem: TStaticItem);
begin
  FScreenBuffer.Delete(AStaticItem);
  FRepaintNeeded := True;
  ForceUpdateCurrentTile;
  InvalidateFilter
end;

procedure TfrmMain.OnStaticElevated(AStaticItem: TStaticItem);
var
  blockInfo: PBlockInfo;
begin
  AStaticItem.PrioritySolver := FScreenBuffer.GetSerial;
  blockInfo := FScreenBuffer.UpdateSortOrder(AStaticItem);
  if blockInfo <> nil then
  begin
    PrepareScreenBlock(blockInfo);
    Exclude(FScreenBufferState, sbsIndexed);
    ForceUpdateCurrentTile;
    InvalidateFilter
  end;
end;

procedure TfrmMain.OnStaticHued(AStaticItem: TStaticItem);
var
  blockInfo: PBlockInfo;
begin
  blockInfo := nil;
  while FScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.Item = AStaticItem then
    begin
      PrepareScreenBlock(blockInfo);
      FRepaintNeeded := True;
      ForceUpdateCurrentTile;
      Break;
    end;
  end;
end;

procedure TfrmMain.OnStaticInserted(AStaticItem: TStaticItem);
begin
  if (AStaticItem.X >= FX + FLowOffsetX) and
     (AStaticItem.X <= FX + FHighOffsetX) and
     (AStaticItem.Y >= FY + FLowOffsetY) and
     (AStaticItem.Y <= FY + FHighOffsetY) then
  begin
    AStaticItem.PrioritySolver := FScreenBuffer.GetSerial;
    PrepareScreenBlock(FScreenBuffer.Insert(AStaticItem));
    FRepaintNeeded := True;
    ForceUpdateCurrentTile;
    InvalidateFilter
  end;
end;

function  TfrmMain.LoadListError(condition: Boolean; filename, message : string): Boolean; inline;
var
  answer: Word;
  l, c: Word;
begin
  result := False;
  if not condition
    then Exit;

  l := Length(filename);
  for c := l downto 1 do
    if (filename[c] = '\') or (filename[c] = '/')
      then break;
  if c > 1 then inc(c);
  answer := MessageDlg(Format(GetParseErText('MessageDlgCaption'), [Copy(filename, c, l-c+1)]),
            Format(GetParseErText('MessageDlgTxtLine1')+sLineBreak+
            GetParseErText('MessageDlgTxtLine2')+sLineBreak+sLineBreak+
            GetParseErText('MessageDlgTxtLine3'),
            [message, filename]), mtError, [mbAbort, mbRetry, mbIgnore], 0);
  if answer = mrAbort
    then begin
      Application.Terminate;
      Halt;
    end;
  result := (answer = mrRetry);
end;

procedure TfrmMain.LoadVisibleTiles(AFileName: String);
var
  XMLDoc:  TXMLDocument;
  iNode, node: TDOMNode;
  s: string;
  i, id: Integer;
begin
  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['VirtualTiles.xml']));
  FVisibleTiles := TBits.Create($4000 + FLandscape.MaxStaticID);
  for i := 0 to FVisibleTiles.Size - 1 do
    FVisibleTiles[i] := True;

  // Читаем xml файл с жесткого диска
  ReadXMLFile(XMLDoc, AFileName);
  if LowerCase(XMLDoc.DocumentElement.NodeName) = 'virtualtiles' then
  begin
    iNode := XMLDoc.DocumentElement.FirstChild;
    while iNode <> nil do
    begin
      if LowerCase(iNode.NodeName) = 'unused' then
      begin
        node := iNode.FirstChild;
        while node <> nil do
        begin
          s := LowerCase(node.NodeName);
          if (s = 'tile') or (s = 'land') or (s = 'item') then
            for i := node.Attributes.Length - 1 downto 0 do
              if LowerCase(node.Attributes[i].NodeName) = 'id' then
                if TryStrToInt(node.Attributes[i].NodeValue, id) then
                begin
                  if s = 'item'
                    then Inc(id, $4000);
                  if (id >= 0) and (id < FVisibleTiles.Size)
                    then FVisibleTiles[id] := False;
                end;
          node := node.NextSibling;
        end;
      end;
      iNode := iNode.NextSibling;
    end;
  end;
end;

procedure TfrmMain.LoadLightSourceTiles(AFileName: String);
var
  XMLDoc:  TXMLDocument;
  iNode, node: TDOMNode;
  s: string;
  i, id, bit: Integer;
begin
  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['VirtualTiles.xml']));
  if (FLightSourceTiles = nil) then begin
    getmem(FLightSourceTiles, (FLandscape.MaxStaticID + 1) * SizeOf(TLightTile));
    for i := 0 to FLandscape.MaxStaticID - 1 do
      FillByte(FLightSourceTiles[i], SizeOf(TLightTile), 0);
  end;

  // Читаем xml файл с жесткого диска
  ReadXMLFile(XMLDoc, AFileName);
  if LowerCase(XMLDoc.DocumentElement.NodeName) = 'virtualtiles' then
  begin
    iNode := XMLDoc.DocumentElement.FirstChild;
    while iNode <> nil do
    begin
      if LowerCase(iNode.NodeName) = 'lightsource' then
      begin
        node := iNode.FirstChild;
        while node <> nil do
        begin
          s := LowerCase(node.NodeName);
          if (s = 'tile') or (s = 'item') then begin
            id  :=-1;
            bit := 1;
            for i := node.Attributes.Length - 1 downto 0 do
            begin
              if LowerCase(node.Attributes[i].NodeName) = 'id' then
                if TryStrToInt(node.Attributes[i].NodeValue, id) then
                begin
                  if s = 'tile'
                    then Inc(id, -$4000);
                  //if (id >= 0) and (id < FLightSourceTiles.Size)
                  //  then FLightSourceTiles[id] := True;
                end;
              if LowerCase(node.Attributes[i].NodeName) = 'icon' then
                if TryStrToInt(node.Attributes[i].NodeValue, bit) then
                  if (bit < 1) or (bit > FVLightSrcImageCount) then
                    bit := 1;
            end;
            if (id >= 0) and (id <= FLandscape.MaxStaticID) then
            begin
               FLightSourceTiles[id].image := bit;
            end;
          end;
          node := node.NextSibling;
        end;
      end;
      iNode := iNode.NextSibling;
    end;
  end;

end;

procedure TfrmMain.LoadSurfsTilesList;
begin
  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['SurfaceInf.xml']));
end;

{

procedure TfrmMain.LoadSurfsTilesList;
var
  fPath, str, attribute : string;
  XMLDoc: TXMLDocument;
  cNode, sNode, eNode : TDOMNode;
  c, s, e, a : Integer;

  tCount: Integer;
  tHash1: ^DWORD;
  tHash2: ^DWORD;
  tIndex: ^LongWord;


value : Integer;
  //e, t, a, value : Integer;

//  cCount :   Word;
  sCount :  ^Word;
  eCount : ^PWord;
begin
  if FileExists(FProfileDir + 'SurfaceInf.xml')
    then fPath := (FProfileDir + 'SurfaceInf.xml')
    else if FileExists(FLocalDir + 'SurfaceInf.xml')
      then fPath := (FLocalDir + 'SurfaceInf.xml')
      else if FileExists(FConfigDir + 'SurfaceInf.xml')
        then fPath := (FConfigDir + 'SurfaceInf.xml')
        else if FileExists(ResMan.GetFile('SurfaceInf.xml'))
          then fPath := (ResMan.GetFile('SurfaceInf.xml'))
          else Exit;





  // Читаем xml файл с жесткого диска
  ReadXMLFile(XMLDoc, fPath);
  if LowerCase(XMLDoc.DocumentElement.NodeName) = 'surfaceinf' then
  begin
    // Подсчет категорий
    FSurfsList.GroupCount := 0;
    cNode := XMLDoc.DocumentElement.FirstChild;
    while cNode <> nil do
    begin
      if LowerCase(cNode.NodeName) = 'category' then
        inc(FSurfsList.GroupCount);
      cNode := cNode.NextSibling;
    end;
    getmem(sCount, FSurfsList.GroupCount * SizeOf( Word));
    getmem(eCount, FSurfsList.GroupCount * SizeOf(PWord));

    // Подсчет объектов
    cNode := XMLDoc.DocumentElement.FirstChild;
    while cNode <> nil do
    begin
      if LowerCase(cNode.NodeName) = 'category' then
      begin
        sNode := cNode.FirstChild;
        while sNode <> nil do
        begin



          sNode := sNode.NextSibling;
        end;
      end;

      cNode := cNode.NextSibling;
    end;
    ////////////////////////////////////////////////////////////////////////////


    // Создание списка
    FSurfsList.TilesCount := $4000 + FLandscape.MaxStaticID;
    getmem(FSurfsList.Tiles, FSurfsList.TilesCount * SizeOf(PSurfGrad));
    getmem(FSurfsList.Grads, FSurfsList.GradsCount * SizeOf(TSurfGrad));
    getmem(FSurfsList.Group, FSurfsList.GroupCount * SizeOf(TSurfGroup));

    tCount := 0;

    c := 0; // -------------- 1
    cNode := XMLDoc.DocumentElement.FirstChild;
    while cNode <> nil do
    begin
      if LowerCase(cNode.NodeName) = 'category' then begin
        FSurfsList.Group[c].Name := 'NoName';
        for a := cNode.Attributes.Length - 1 downto 0 do begin
          if LowerCase(cNode.Attributes[a].NodeName) = 'name' then
            FSurfsList.Group[c].Name := CP1251ToUTF8(cNode.Attributes[a].NodeValue);
        end;
        // ?????? FSurfsList.Group[c].Count
        getmem(FSurfsList.Group[c].Info, FSurfsList.Group[c].Count * SizeOf(TSurfGroup));

        s := 0; // -------------- 2
        sNode := cNode.FirstChild;
        while sNode <> nil do
        begin
          if LowerCase(sNode.NodeName) = 'surafece' then begin
            FSurfsList.Group[c].Info[s].Name := 'NoName';
            FSurfsList.Group[c].Info[s].TileID := $4001;
            for a := sNode.Attributes.Length - 1 downto 0 do begin
              if LowerCase(sNode.Attributes[a].NodeName) = 'name' then
                FSurfsList.Group[c].Info[s].Name := CP1251ToUTF8(sNode.Attributes[a].NodeValue);
              if LowerCase(sNode.Attributes[a].NodeName) = 'tileid' then
                if TryStrToInt(sNode.Attributes[a].NodeValue, value) then
                  FSurfsList.Group[c].Info[s].TileID := value;
            end;
            CalcStringCRC32(LowerCase(FSurfsList.Group[c].Info[s].Name),
                            FSurfsList.Group[c].Info[s].TileHash);
            // ?????? FSurfsList.Group[c].Info[s].GradCount
            getmem(FSurfsList.Group[c].Info[s].GradHash, FSurfsList.Group[c].Info[s].GradCount * SizeOf(LongWord));

            e := 0; // -------------- 3
            eNode := sNode.FirstChild;
            while eNode <> nil do
            begin
              str := LowerCase(eNode.NodeName);
              if (str = 'item') or (str = 'land') or (str = 'tile') then begin
                str := 'NoType';
                for a := eNode.Attributes.Length - 1 downto 0 do begin
                  if LowerCase(sNode.Attributes[a].NodeName) = 'type' then
                    str := CP1251ToUTF8(eNode.Attributes[a].NodeValue);
                end;
                CalcStringCRC32(LowerCase(FSurfsList.Group[c].Name + '|' + str),
                                FSurfsList.Group[c].Info[s].GradHash[e]);
                inc(tCount);
              end else if str = 'brush' then begin
                // TODO

              end;
              inc(e);
              eNode := eNode.NextSibling;
            end;    // -------------- 3

          end;
          inc(s);
          sNode := sNode.NextSibling;
        end;    // -------------- 2

      end;
      inc(c);
      cNode := cNode.NextSibling;
    end;    // -------------- 1

    ////////////////////////////////////////////////////////////////////////////

    // Построение списка TSurfGrad
    getmem(tHash, tCount * SizeOf(DWORD));
    tCount := 0;
    for c := 0 to FSurfsList.GroupCount do
      for s := 0 to FSurfsList.Group[c].Count do
        for e := 0 to FSurfsList.Group[c].Info[s].GradCount do
        begin
          for a := tCount-1 downto -1 do
            if a < 0 then begin
              tHash[tCount] = FSurfsList.Group[c].Info[s].GradHash[e];
              inc(tCount);
            end else if tHash[a] = FSurfsList.Group[c].Info[s].GradHash[e]
                then break;
        end;

    FSurfsList.GradsCount := tCount;
    getmem(FSurfsList.Grads, FSurfsList.GradsCount * SizeOf(TSurfGrad));




    freemem(gHash);



    getmem(sCount, FSurfsList.GroupCount * SizeOf( Word));
    getmem(eCount, FSurfsList.GroupCount * SizeOf(PWord));

    // Подсчет объектов
    cNode := XMLDoc.DocumentElement.FirstChild;
    while cNode <> nil do
    begin
      if LowerCase(cNode.NodeName) = 'category' then
      begin
        sNode := cNode.FirstChild;
        while sNode <> nil do
        begin



          sNode := sNode.NextSibling;
        end;
      end;

      cNode := cNode.NextSibling;
    end;



  end;


end;

}

procedure TfrmMain.LoadEntryTilesList;
var
  fPath, s, attribute : string;
  XMLDoc: TXMLDocument;
  eNode, tNode : TDOMNode;
  e, t, a, value : Integer;
  // Создание миниатюр
  id : Integer;
  hue : Word;
  alt : integer;

  artHue  : THue;
  artEntry : TArt;
  dSize: TSize;
  entHash: QWORD;
  Resised: Float;
  tCRC32, CacheLen: DWORD;
  cacheHash, cachePost: PDWORD;
  imageData: TImageData;
  imageCache: TFileStream;
  imageTemp: TSingleImage;
  dX, dY, dZ, dW, dH : Integer;
  entStack, entFirst, entLast, entPrev, entNext: PByte;
  dstCanvas: TFastARGB32Canvas;
  srcCanvas: TFastARGB32Canvas;
begin
  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['TilesEntry.xml']));
  if FileExists(FProfileDir + 'TilesEntry.xml')
    then fPath := (FProfileDir + 'TilesEntry.xml')
    else if FileExists(FLocalDir + 'TilesEntry.xml')
      then fPath := (FLocalDir + 'TilesEntry.xml')
      else if FileExists(FConfigDir + 'TilesEntry.xml')
        then fPath := (FConfigDir + 'TilesEntry.xml')
        else if FileExists(ResMan.GetFile('TilesEntry.xml'))
          then fPath := (ResMan.GetFile('TilesEntry.xml'))
          else Exit;

  FEntryList.Count := 0;

  // Читаем xml файл с жесткого диска
  ReadXMLFile(XMLDoc, fPath);
  if LowerCase(XMLDoc.DocumentElement.NodeName) = 'tilesentry' then
  begin
    // Подсчет объектов
    eNode := XMLDoc.DocumentElement.FirstChild;
    while eNode <> nil do
    begin
      if LowerCase(eNode.NodeName) = 'entry' then
        inc(FEntryList.Count);
      eNode := eNode.NextSibling;
    end;
    getmem(FEntryList.Entry, FEntryList.Count * SizeOf(PGroupEntry));

    // Построение списка
    eNode := XMLDoc.DocumentElement.FirstChild;
    for e := 0 to FEntryList.Count - 1 do begin
      //Logger.Send([lcInfo], 'Entry: %d / %d - Start...', [e, FEntryList.Count]);

      new (FEntryList.Entry[e]);
      FEntryList.Entry[e]^.Name := 'NoName';
      for a := eNode.Attributes.Length - 1 downto 0 do begin
         if LowerCase(eNode.Attributes[a].NodeName) = 'id' then
           if TryStrToInt(eNode.Attributes[a].NodeValue, value) then
             FEntryList.Entry[e]^.ID := value;
         if LowerCase(eNode.Attributes[a].NodeName) = 'name' then
           FEntryList.Entry[e]^.Name := CP1251ToUTF8(eNode.Attributes[a].NodeValue);
      end;

      // Подсчет тайлов
      tNode := eNode.FirstChild;
      FEntryList.Entry[e]^.Count := 0;
      while tNode <> nil do begin
        s := LowerCase(tNode.NodeName);
        if (s = 'tile') or (s = 'item') or (s = 'land') then
          inc(FEntryList.Entry[e]^.Count);
        tNode := tNode.NextSibling;
      end;
      getmem(FEntryList.Entry[e]^.ETile, FEntryList.Entry[e]^.Count * SizeOf(TEntryTile));

      // Чтение списка тайлов
      tNode := eNode.FirstChild;
      for t := 0 to FEntryList.Entry[e]^.Count - 1 do
      begin
        //Logger.Send([lcInfo], 'Entry: %d - Item: %d / %d', [e, t, FEntryList.Entry[e]^.Count]);
        FEntryList.Entry[e]^.ETile[t].ID  := 0;
        FEntryList.Entry[e]^.ETile[t].Hue := 0;
        FEntryList.Entry[e]^.ETile[t].X   := 0;
        FEntryList.Entry[e]^.ETile[t].Y   := 0;
        FEntryList.Entry[e]^.ETile[t].Z   := 0;
        for a := tNode.Attributes.Length - 1 downto 0 do begin
          attribute := LowerCase(tNode.Attributes[a].NodeName);
          if attribute = 'id' then begin
            if TryStrToInt(tNode.Attributes[a].NodeValue, value) then begin
              if s = 'item' then Inc(value, $4000);
              FEntryList.Entry[e]^.ETile[t].ID := value;
            end;
          end else if attribute = 'hue' then begin
            if TryStrToInt(tNode.Attributes[a].NodeValue, value)
              then FEntryList.Entry[e]^.ETile[t].Hue := value;
          end else if attribute = 'x' then begin
            if TryStrToInt(tNode.Attributes[a].NodeValue, value)
              then FEntryList.Entry[e]^.ETile[t].X := value;
          end else if attribute = 'y' then begin
            if TryStrToInt(tNode.Attributes[a].NodeValue, value)
              then FEntryList.Entry[e]^.ETile[t].Y := value;
          end else if attribute = 'z' then begin
            if TryStrToInt(tNode.Attributes[a].NodeValue, value)
              then FEntryList.Entry[e]^.ETile[t].Z := value;
          end;
        end;

        while (tNode <> nil) do begin
          tNode := tNode.NextSibling;
          if tNode <> nil then begin
            s := LowerCase(tNode.NodeName);
            if (s = 'tile') or (s = 'item') or (s = 'land')
            then break;
          end;
        end;

      end;

      while (eNode <> nil) do begin
        eNode := eNode.NextSibling;
        if (eNode <> nil) and (LowerCase(eNode.NodeName) = 'entry')
        then break;
      end;
    end;
  end;
  XMLDoc.Free;

  Logger.Send([lcInfo], 'TfrmMain.LoadEntryTilesList SizeOf(TImageFormat)', SizeOf(TImageFormat));
  // Создание миниатюр
  if (FProfileDir <> '')
    then fPath := FProfileDir
    else fPath := FConfigDir;
  fPath := fPath + 'TilesEntry.cache';
  if not FileExists(fPath)
     then FileClose(FileCreate(fPath));
  imageCache := TFileStream.Create(fPath, fmOpenReadWrite);
  if (imageCache.Size < 4) then begin
    value := 0;
    cacheHash := nil;
    cachePost := nil;
    imageCache.WriteDWord(value);
  end else begin
    value := imageCache.ReadDWord;
    getmem(cacheHash, 4 * value);
    getmem(cachePost, 4 * value);
  end;

  t := 0;
  CacheLen := value;
  while (t < value) do begin
    cacheHash[t] := imageCache.ReadDWord;
    cachePost[t] := imageCache.Position;
    imageCache.Position := imageCache.Position + 12;
    e := imageCache.ReadDWord;
    imageCache.Position := imageCache.Position + e;
    inc(t);
  end;

  for e := 0 to FEntryList.Count - 1 do begin
    s := Format('e:%d|n:%s|', [FEntryList.Entry[e]^.ID, FEntryList.Entry[e]^.Name]);
    for t := 0 to FEntryList.Entry[e]^.Count - 1 do
        s := s + Format('i:%d|h:%d|x:%d|y:%d|z:%d|', [FEntryList.Entry[e]^.ETile[t].ID, FEntryList.Entry[e]^.ETile[t].Hue,
                     FEntryList.Entry[e]^.ETile[t].X, FEntryList.Entry[e]^.ETile[t].Y, FEntryList.Entry[e]^.ETile[t].Z]);
    CalcStringCRC32(s, tCRC32);
    FEntryList.Entry[e]^.Image := nil;
    // Чтение изображения из кэша
    for t := 0 to value - 1 do
      if (cacheHash[t] = tCRC32) then begin
        imageCache.Position := cachePost[t];
        imageCache.Read(imageData, 16);
        getmem(imageData.Bits, imageData.Size);
        imageCache.Read(PByte(imageData.Bits)^, imageData.Size);
        imageData.Palette := nil;
        FEntryList.Entry[e]^.Image := TSingleImage.CreateFromData(imageData);
        freemem(imageData.Bits);
        //Logger.Send([lcInfo], 'TfrmMain.LoadEntryTilesList entry %d Loaded from Cache', [FEntryList.Entry[e]^.ID]);
        //FEntryList.Entry[e]^.Image.Format := ifA1R5G5B5;
        Break;
      end;
    if (FEntryList.Entry[e]^.Image <> nil)
      then Continue;

    // Сортировка тайлов для отрисовки
    dW := 0;
    dH := 0;
    alt:= 0;
    getmem(entStack, FEntryList.Entry[e]^.Count * 12);
    FillByte(entStack^, FEntryList.Entry[e]^.Count * 12, 0);
    entFirst := entStack;
    entLast  := entStack;
    for t := 0 to FEntryList.Entry[e]^.Count - 1 do begin
      dSize := ResMan.Art.GetArtSize(FEntryList.Entry[e]^.ETile[t].ID);
      dX :=  44 * (FEntryList.Entry[e]^.ETile[t].X - FEntryList.Entry[e]^.ETile[t].Y);
      dY := -22 * (FEntryList.Entry[e]^.ETile[t].X + FEntryList.Entry[e]^.ETile[t].Y) + 4 * FEntryList.Entry[e]^.ETile[t].Z;
      if (dY >= 0) then begin
        dH := max(dH, alt + dY + dSize.cy);
      end else begin
        a := alt;
        alt:= max(alt, Abs(dY));
        dH := alt + max(dH - a, max(0, dSize.cy + dY));
      end;
      dW := max(dW, Abs(dX) + dSize.cx);
      dX := min(max(0, FEntryList.Entry[e]^.ETile[t].X + 1023), 2047);
      dY := min(max(0, FEntryList.Entry[e]^.ETile[t].Y + 1023), 2047);
      dZ := min(max(0, FEntryList.Entry[e]^.ETile[t].Z +  255),  513);

      entHash := DWORD(dX) * 1064440 + DWORD(dY) * 520 + DWORD(dZ + 7);
      if (tdfFoliage in ResMan.Tiledata.TileData[FEntryList.Entry[e]^.ETile[t].ID].Flags)
      then Inc(entHash, +1);
      PDWord(entLast)^ := entHash;
      PDWord(entLast + 4)^ := DWord(t);

      if (entLast = entStack) then begin
        inc(entLast, 12);
      end else begin
        entNext := entFirst;
        entPrev  := nil;
        while True do begin
          if (entNext = nil) then begin
            PDWord(entPrev + 8)^ := PtrInt(entLast);
            inc(entLast, 12);
            Break;
          end else if (PDWord(entNext)^ >= entHash) then begin
            if (entPrev <> nil) then begin
              PDWord(entPrev + 8)^ := PtrInt(entLast);
            end else begin
              entFirst := entLast;
            end;
            PDWord(entLast + 8)^ := PtrInt(entNext);
            inc(entLast, 12);
            Break;
          end;
          entPrev := entNext;
          entNext := PByte(PDWord(entNext + 8)^);
        end;
      end;
    end;

    // Отрисовка изображения миниатюры
    //Logger.Send([lcInfo], 'TfrmMain.LoadEntryTilesList %d dW: %d dH: %d', [FEntryList.Entry[e]^.ID, dW, dH]);
    FEntryList.Entry[e]^.Image := TSingleImage.CreateFromParams(dW, dH, ifA8R8G8B8);
    dstCanvas := TFastARGB32Canvas.CreateForImage(FEntryList.Entry[e]^.Image);
    dstCanvas.FillColor32 := $00404040;
    dstCanvas.FillRect(Rect(0,0,FEntryList.Entry[e]^.Image.Width,FEntryList.Entry[e]^.Image.Height));
    entNext := entFirst;
    while (entNext <> nil) do begin

//if ((FEntryList.Entry[e]^.ID < 9120) and (FEntryList.Entry[e]^.ID > 9025))
//then break;
      t := PDWord(entNext + 4)^;
      //Logger.Send([lcInfo], 'TfrmMain.LoadEntryTilesList t: %d [%d/%d]', [t, alt, FEntryList.Entry[e]^.Image.Height]);
      entNext := PByte(PDWord(entNext + 8)^);
      id  := FEntryList.Entry[e]^.ETile[t].ID;
      hue := FEntryList.Entry[e]^.ETile[t].Hue;
      if ResMan.Art.Exists(id) then begin
        if hue > 0
           then artHue := ResMan.Hue.Hues[hue - 1]
           else artHue := nil;

        artEntry := ResMan.Art.GetArt(id, EncodeUOColor($0FF0F000), artHue, tdfPartialHue in TTileData(ResMan.Tiledata.Block[id]).Flags);
        srcCanvas := TFastARGB32Canvas.CreateForImage(artEntry.Graphic);
        dX := (FEntryList.Entry[e]^.Image.Width - artEntry.Graphic.Width) div 2 + 22 * (FEntryList.Entry[e]^.ETile[t].X - FEntryList.Entry[e]^.ETile[t].Y);
        dY := FEntryList.Entry[e]^.Image.Height - artEntry.Graphic.Height - alt +
                 22 * (FEntryList.Entry[e]^.ETile[t].X + FEntryList.Entry[e]^.ETile[t].Y) - 4 * FEntryList.Entry[e]^.ETile[t].Z;
        srcCanvas.DrawAlpha(Rect(0,0,artEntry.Graphic.Width,artEntry.Graphic.Height), dstCanvas, dX,dY);
        srcCanvas.Free;
        artEntry.Free;
      end;
    end;


    // Уменьшение изоображения
    dW := 93;
    dH := 96;
    dSize.cx := FEntryList.Entry[e]^.Image.Width;
    dSize.cy := FEntryList.Entry[e]^.Image.Height;
    Resised := Min(Min(Float(dW)/Float(dSize.cx), Float(dH)/Float(dSize.cy)), 1.0);
    dSize.cx := Trunc(Resised * Float(dSize.cx));
    dSize.cy := Trunc(Resised * Float(dSize.cy));
    imageTemp := TSingleImage.CreateFromParams(dSize.cx, dSize.cy, ifA8R8G8B8);
    srcCanvas := TFastARGB32Canvas.CreateForImage(imageTemp);
    dstCanvas.StretchDrawAdd(Rect(0,0,FEntryList.Entry[e]^.Image.Width,FEntryList.Entry[e]^.Image.Height),
                             srcCanvas, Rect(0,0,dSize.cx,dSize.cy), rfNearest);//rfBilinear);
    srcCanvas.Free;
    dstCanvas.Free;
    freemem(entStack);
    FEntryList.Entry[e]^.Image.Free;
    FEntryList.Entry[e]^.Image := imageTemp;

    // Сохранение изоображение в кэше
    imageCache.Position := imageCache.Size;
    imageCache.WriteDWord(tCRC32);
    imageData := FEntryList.Entry[e]^.Image.ImageDataPointer^;
    imageCache.Write(imageData, 16);
    imageCache.Write(imageData.Bits^, imageData.Size);
    inc(CacheLen, +1);
    //Logger.Send([lcInfo], 'TfrmMain.LoadEntryTilesList imageData.Palette <> nil', imageData.Palette <> nil);

    //
    Application.ProcessMessages;
  end;

  if (value > 0) then begin
    freemem(cacheHash);
    freemem(cachePost);
  end;
  imageCache.Position := 0;
  imageCache.WriteDWord(CacheLen);
  imageCache.Free;

  { // Отладка - проверка считанного файла
  Logger.EnterMethod([lcInfo, lcDebug], 'TfrmMain.LoadEntryTilesList');
  Logger.Send([lcInfo], 'Entries Number: %x', [FEntryList.Count]);
  for e := 0 to FEntryList.Count - 1 do begin
    Logger.Send([lcInfo], '<Entry Id="%.4d" Name="%s" Tiles="%x">',
      [FEntryList.Entry[e]^.ID, FEntryList.Entry[e]^.Name, FEntryList.Entry[e]^.Count]);
    for t := 0 to FEntryList.Entry[e]^.Count - 1 do
      Logger.Send([lcInfo], '    <Tile Id="0x%.5x" Hue="0x%.3x" X="%d" Y="%d" Z="%d">',
        [FEntryList.Entry[e]^.ETile[t].ID, FEntryList.Entry[e]^.ETile[t].Hue,
        FEntryList.Entry[e]^.ETile[t].X, FEntryList.Entry[e]^.ETile[t].Y, FEntryList.Entry[e]^.ETile[t].Z]);
    Logger.Send([lcInfo], '</Entry>');
  end;
  Logger.ExitMethod([lcInfo, lcDebug], 'TfrmMain.LoadEntryTilesList');
  }
end;

procedure TfrmMain.LoadBrushTilesList;
var
  fPath, s, attribute : string;
  XMLDoc: TXMLDocument;
  bNode, tNode, eNode : TDOMNode;
  z, b, t, a, e, value: Integer;
  uu, ur, ll, ul, dl, dr : Word;
  valueF : Single;
  brushTile: TBrushTile;
  // Создание миниатюр
  id : Integer;
  destColor, hue : Word;
  destRect: TRect;

  texmEntry : TTexture;
  dX, dY, dW, dH : Integer;
begin
  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['TilesBrush.xml']));
  if FileExists(FProfileDir + 'TilesBrush.xml')
    then fPath := (FProfileDir + 'TilesBrush.xml')
    else if FileExists(FLocalDir + 'TilesBrush.xml')
      then fPath := (FLocalDir + 'TilesBrush.xml')
      else if FileExists(FConfigDir + 'TilesBrush.xml')
        then fPath := (FConfigDir + 'TilesBrush.xml')
        else if FileExists(ResMan.GetFile('TilesBrush.xml'))
          then fPath := (ResMan.GetFile('TilesBrush.xml'))
          else Exit;

  FBrushList.Count := 0;

  // Читаем xml файл с жесткого диска
  ReadXMLFile(XMLDoc, fPath);
  if LowerCase(XMLDoc.DocumentElement.NodeName) = 'tilesbrush' then
  begin
    // Подсчет объектов
    bNode := XMLDoc.DocumentElement.FirstChild;
    while bNode <> nil do
    begin
      if LowerCase(bNode.NodeName) = 'brush' then
        inc(FBrushList.Count);
      bNode := bNode.NextSibling;
    end;
    getmem(FBrushList.Brush, FBrushList.Count * SizeOf(PGroupBrush));

    // Построение списка
    bNode := XMLDoc.DocumentElement.FirstChild;
    for b := 0 to FBrushList.Count - 1 do begin
      //Logger.Send([lcInfo], 'Brush: %d / %d - Start...', [b+1, FBrushList.Count]);
      new (FBrushList.Brush[b]);
      FBrushList.Brush[b]^.ID := 0;
      FBrushList.Brush[b]^.Name := 'NoName';
      for a := bNode.Attributes.Length - 1 downto 0 do begin
         if LowerCase(bNode.Attributes[a].NodeName) = 'id' then
           if TryStrToInt(bNode.Attributes[a].NodeValue, value) then
             FBrushList.Brush[b]^.ID := value;
         if LowerCase(bNode.Attributes[a].NodeName) = 'name' then
           FBrushList.Brush[b]^.Name := CP1251ToUTF8(bNode.Attributes[a].NodeValue);
      end;

      // Тестирование...
      if LoadListError((FBrushList.Brush[b]^.ID = 0) or (FBrushList.Brush[b]^.ID > 9999),
         fPath, Format(GetParseErText('blTagBrushAttrID'),  [FBrushList.Brush[b]^.ID, FBrushList.Brush[b]^.Name]))
         //'Missmatch or wrong <ID> attribute in BrushID: %.4d ("%s"). Brush ID must be larger  0 and less than 9999', [FBrushList.Brush[b]^.ID, FBrushList.Brush[b]^.Name]))
         then begin LoadBrushTilesList; Exit; end;
      if b > 0 then for z := b-1 downto 0 do begin
        if LoadListError(FBrushList.Brush[z]^.ID = FBrushList.Brush[b]^.ID,
           fPath, Format(GetParseErText('blTagBrushDuplicate'), [FBrushList.Brush[b]^.ID]))
           then begin LoadBrushTilesList; Exit; end;
      end;

      // Подсчет тайлов и переходов
      tNode := bNode.FirstChild;
      FBrushList.Brush[b]^.Count  := 0;
      FBrushList.Brush[b]^.ECount := 0;
      while tNode <> nil do begin
        s := LowerCase(tNode.NodeName);
        if (s = 'brushTile') or (s = 'land') then
          inc(FBrushList.Brush[b]^.Count)
        else if (s = 'edge') then
          inc(FBrushList.Brush[b]^.ECount);
        tNode := tNode.NextSibling;
      end;
      getmem(FBrushList.Brush[b]^.BTile,  FBrushList.Brush[b]^.Count  * SizeOf(PBrushTile));
      getmem(FBrushList.Brush[b]^.EdgeId, FBrushList.Brush[b]^.ECount * SizeOf(PWord));
      getmem(FBrushList.Brush[b]^.BEdges, FBrushList.Brush[b]^.ECount * SizeOf(PGroupBrushEdge));

      // Тестирование...
      if LoadListError(FBrushList.Brush[b]^.Count = 0,
         fPath, Format(GetParseErText('blTagBrushEmpty'), [FBrushList.Brush[b]^.ID]))
         then begin LoadBrushTilesList; Exit; end;


      // Чтение списка тайлов
      tNode := bNode.FirstChild;
      t := 0; e := 0;
      while tNode <> nil do
      begin
        s := LowerCase(tNode.NodeName);
        if (s = 'brushTile') or (s = 'land') then begin
          //Logger.Send([lcInfo], 'Brush: %d - Land: %d / %d', [b+1, t+1, FBrushList.Brush[b]^.Count]);
          brushTile.ID := $FFFF;
          brushTile.Chance := 1.0;
          for a := tNode.Attributes.Length - 1 downto 0 do begin
            attribute := LowerCase(tNode.Attributes[a].NodeName);
            if attribute = 'id' then begin
              if TryStrToInt(tNode.Attributes[a].NodeValue, value)
                then brushTile.ID := value;
            end else if attribute = 'chance' then begin
              if TryStrToFloat(tNode.Attributes[a].NodeValue, valueF)
                then brushTile.Chance := valueF;
            end;
          end;
          brushTile.Mask   := $0F;
          brushTile.Brush1 := FBrushList.Brush[b];
          brushTile.Brush2 := FBrushList.Brush[b];

          // Тестирование...
          if LoadListError(brushTile.ID = $FFFF,
             fPath, Format(GetParseErText('blTagTileAttrID'), [FBrushList.Brush[b]^.ID]))
             then begin LoadBrushTilesList; Exit; end;
          if LoadListError(brushTile.ID > $3FFF,
             fPath, Format(GetParseErText('blTagTileAttrIDOutOfRange'), [brushTile.ID, brushTile.ID, FBrushList.Brush[b]^.ID]))
             then begin LoadBrushTilesList; Exit; end;
          if LoadListError(FBrushList.Tiles[brushTile.ID].ID = brushTile.ID,
             fPath, Format(GetParseErText('blTagTileRedeclaration'), [brushTile.ID, brushTile.ID, FBrushList.Brush[b]^.ID]))
             then begin LoadBrushTilesList; Exit; end;

          FBrushList.Tiles[brushTile.ID] := brushTile;
          FBrushList.Brush[b]^.BTile[t] := @FBrushList.Tiles[brushTile.ID];
          inc(t);
        end else if  (s = 'edge') then begin
          //Logger.Send([lcInfo], 'Brush: %d - Edge: %d / %d', [b+1, e+1, FBrushList.Brush[b]^.ECount]);
          //new (FBrushList.Brush[b]^.EdgeId[e]);
          new (FBrushList.Brush[b]^.BEdges[e]);
          FBrushList.Brush[b]^.BEdges[e]^.ID := 0;
          for a := tNode.Attributes.Length - 1 downto 0 do begin
            attribute := LowerCase(tNode.Attributes[a].NodeName);
            if attribute = 'to' then begin
              if TryStrToInt(tNode.Attributes[a].NodeValue, value)
                then FBrushList.Brush[b]^.BEdges[e]^.ID := value;
            end;
          end;
          // Тестирование...
          if LoadListError((FBrushList.Brush[b]^.BEdges[e]^.ID = 0) or (FBrushList.Brush[b]^.BEdges[e]^.ID > 9999),
             fPath, Format(GetParseErText('blTagEdgeAttrTo'), [FBrushList.Brush[b]^.ID]))
             then begin LoadBrushTilesList; Exit; end;

          // Подсчет тайлов в переходе
          eNode := tNode.FirstChild;
          FBrushList.Brush[b]^.BEdges[e]^.CountUU := 0;
          FBrushList.Brush[b]^.BEdges[e]^.CountUR := 0;
          FBrushList.Brush[b]^.BEdges[e]^.CountLL := 0;
          FBrushList.Brush[b]^.BEdges[e]^.CountUL := 0;
          FBrushList.Brush[b]^.BEdges[e]^.CountDL := 0;
          FBrushList.Brush[b]^.BEdges[e]^.CountDR := 0;
          while eNode <> nil do begin
            s := LowerCase(eNode.NodeName);
            if (s = 'brushTile') or (s = 'land') then begin
              attribute := '';
              for a := eNode.Attributes.Length - 1 downto 0 do begin
                attribute := LowerCase(eNode.Attributes[a].NodeName);
                if attribute = 'type' then begin
                  s := LowerCase(CP1251ToUTF8(eNode.Attributes[a].NodeValue));
                  if      s = 'uu' then inc(FBrushList.Brush[b]^.BEdges[e]^.CountUU)
                  else if s = 'ur' then inc(FBrushList.Brush[b]^.BEdges[e]^.CountUR)
                  else if s = 'll' then inc(FBrushList.Brush[b]^.BEdges[e]^.CountLL)
                  else if s = 'ul' then inc(FBrushList.Brush[b]^.BEdges[e]^.CountUL)
                  else if s = 'dl' then inc(FBrushList.Brush[b]^.BEdges[e]^.CountDL)
                  else if s = 'dr' then inc(FBrushList.Brush[b]^.BEdges[e]^.CountDR);
                  break;
                end;
              end;

              if LoadListError((attribute<>'type') or ((s<>'uu')and(s<>'ur')and(s<>'ll')and(s<>'ul')and(s<>'dl')and(s<>'dr')),
                 fPath, Format(GetParseErText('blTagTile2AttrType'), [brushTile.ID, brushTile.ID, FBrushList.Brush[b]^.BEdges[e]^.ID, FBrushList.Brush[b]^.ID]))
                 then begin LoadBrushTilesList; Exit; end;
            end;
            eNode := eNode.NextSibling;
          end;
          getmem(FBrushList.Brush[b]^.BEdges[e]^.BTileUU,  FBrushList.Brush[b]^.BEdges[e]^.CountUU  * SizeOf(PBrushTile));
          getmem(FBrushList.Brush[b]^.BEdges[e]^.BTileUR,  FBrushList.Brush[b]^.BEdges[e]^.CountUR  * SizeOf(PBrushTile));
          getmem(FBrushList.Brush[b]^.BEdges[e]^.BTileLL,  FBrushList.Brush[b]^.BEdges[e]^.CountLL  * SizeOf(PBrushTile));
          getmem(FBrushList.Brush[b]^.BEdges[e]^.BTileUL,  FBrushList.Brush[b]^.BEdges[e]^.CountUL  * SizeOf(PBrushTile));
          getmem(FBrushList.Brush[b]^.BEdges[e]^.BTileDL,  FBrushList.Brush[b]^.BEdges[e]^.CountDL  * SizeOf(PBrushTile));
          getmem(FBrushList.Brush[b]^.BEdges[e]^.BTileDR,  FBrushList.Brush[b]^.BEdges[e]^.CountDR  * SizeOf(PBrushTile));
          // Чтение списка тайлов в переходе
          uu := 0; ur := 0; ll := 0; ul := 0; dl := 0; dr := 0;
          eNode := tNode.FirstChild;
          while eNode <> nil do begin
            s := LowerCase(eNode.NodeName);
            if (s = 'brushTile') or (s = 'land') then begin
              //Logger.Send([lcInfo], 'Brush: %d - Edge: %d - Land: %d / %d', [b+1, e+1, uu+ur+ll+ul+dl+dr+1, FBrushList.Brush[b]^.BEdges[e]^.CountUU+FBrushList.Brush[b]^.BEdges[e]^.CountUR+FBrushList.Brush[b]^.BEdges[e]^.CountLL+FBrushList.Brush[b]^.BEdges[e]^.CountUL+FBrushList.Brush[b]^.BEdges[e]^.CountDL+FBrushList.Brush[b]^.BEdges[e]^.CountDR]);
              brushTile.ID := $FFFF;
              brushTile.Chance := 1.0;
              for a := eNode.Attributes.Length - 1 downto 0 do begin
                attribute := LowerCase(eNode.Attributes[a].NodeName);
                if attribute = 'type' then begin
                  s := LowerCase(CP1251ToUTF8(eNode.Attributes[a].NodeValue));
                  if      s = 'uu' then brushTile.Mask := $03
                  else if s = 'ur' then brushTile.Mask := $07
                  else if s = 'll' then brushTile.Mask := $09
                  else if s = 'ul' then brushTile.Mask := $0B
                  else if s = 'dl' then brushTile.Mask := $0D
                  else if s = 'dr' then brushTile.Mask := $0E;
                end else if attribute = 'id' then begin
                  if TryStrToInt(eNode.Attributes[a].NodeValue, value)
                    then brushTile.ID := value;
                end else if attribute = 'chance' then begin
                  if TryStrToFloat(eNode.Attributes[a].NodeValue, valueF)
                    then brushTile.Chance := valueF;
                end;
              end;

              // Тестирование...
              if LoadListError(brushTile.ID = $FFFF,
                 fPath, Format(GetParseErText('blTagTile2AttrID'), [FBrushList.Brush[b]^.ID]))
                 then begin LoadBrushTilesList; Exit; end;
              if LoadListError(brushTile.ID > $3FFF,
                 fPath, Format(GetParseErText('blTagTile2AttrIDOutOfRange'), [brushTile.ID, brushTile.ID, FBrushList.Brush[b]^.ID]))
                 then begin LoadBrushTilesList; Exit; end;
              if LoadListError(FBrushList.Tiles[brushTile.ID].ID = brushTile.ID,
                 fPath, Format(GetParseErText('blTagTile2Redeclaration'), [brushTile.ID, brushTile.ID, FBrushList.Brush[b]^.ID]))
                 then begin LoadBrushTilesList; Exit; end;

              brushTile.Brush1 := FBrushList.Brush[b];
              brushTile.Brush2 := nil;
              FBrushList.Tiles[brushTile.ID] := brushTile;
              if          brushTile.Mask = $03 then begin
                FBrushList.Brush[b]^.BEdges[e]^.BTileUU[uu] := @FBrushList.Tiles[brushTile.ID]; inc(uu);
              end else if brushTile.Mask = $07 then begin
                FBrushList.Brush[b]^.BEdges[e]^.BTileUR[ur] := @FBrushList.Tiles[brushTile.ID]; inc(ur);
              end else if brushTile.Mask = $09 then begin
                FBrushList.Brush[b]^.BEdges[e]^.BTileLL[ll] := @FBrushList.Tiles[brushTile.ID]; inc(ll);
              end else if brushTile.Mask = $0B then begin
                FBrushList.Brush[b]^.BEdges[e]^.BTileUL[ul] := @FBrushList.Tiles[brushTile.ID]; inc(ul);
              end else if brushTile.Mask = $0D then begin
                FBrushList.Brush[b]^.BEdges[e]^.BTileDL[dl] := @FBrushList.Tiles[brushTile.ID]; inc(dl);
              end else if brushTile.Mask = $0E then begin
                FBrushList.Brush[b]^.BEdges[e]^.BTileDR[dr] := @FBrushList.Tiles[brushTile.ID]; inc(dr);
              end;
              FBrushList.Tiles[brushTile.ID].ID := FBrushList.Brush[b]^.BEdges[e]^.ID; // Временно запоминаем ID перехода (позже востанавливаем ID тайла)
            end;
            eNode := eNode.NextSibling;
          end;
          FBrushList.Brush[b]^.EdgeId[e] := @(FBrushList.Brush[b]^.BEdges[e]^.ID);
          inc(e);
        end;
        tNode := tNode.NextSibling;
      end;
      bNode := bNode.NextSibling;
    end;
  end;
  XMLDoc.Free;

  // Задание вторичных кистей
  for t := 0 to $3FFF do begin
    if (FBrushList.Tiles[t].Brush1 <> nil) and (FBrushList.Tiles[t].Brush2 = nil)
    then begin
      for b := 0 to FBrushList.Count - 1 do
        if FBrushList.Brush[b]^.ID = FBrushList.Tiles[t].ID then begin
          FBrushList.Tiles[t].Brush2 := FBrushList.Brush[b];
          break;
        end;

      // Тестирование...
      if LoadListError((FBrushList.Tiles[t].Brush2 = nil),
         fPath, Format(GetParseErText('blTagEdgeUnknown'), [FBrushList.Tiles[t].Brush1^.ID, FBrushList.Tiles[t].ID]))
         then begin LoadBrushTilesList; Exit; end;

      FBrushList.Tiles[t].ID := t;
    end else if (FBrushList.Tiles[t].Brush1 = nil) and (FBrushList.Tiles[t].Brush2 = nil)
    then begin // Пустые тайлы
      FBrushList.Tiles[t].ID:= $FFFF;
      FBrushList.Tiles[t].Mask:= $00;
    end;
  end;

  // Тестирование...
//  for t := 0 to $3FFF do
//    if LoadListError((FBrushList.Tiles[t].Brush1 <> nil) and (FBrushList.Tiles[t].Brush2 = nil),
//       fPath, Format(GetParseErText('blTagTile2Redeclaration'), [FBrushList.Brush[b]^.ID]))
//       then begin LoadBrushTilesList; Exit; end;


  // Создание миниатюр
  for b := 0 to FBrushList.Count - 1 do begin
    //FBrushList.Brush[b]^.Image := TTexture(ResMan.Texmaps.Block[t]).Graphic;
    FBrushList.Brush[b]^.Image := TSingleImage.CreateFromParams(96, 96, ifA1R5G5B5);

    //FBrushList.Brush[b]^.Image.Bits .Canvas.Brush.Color := $00FF9000;//DecodeUOColor(destColor);
    //FBrushList.Brush[b]^.Image.Canvas.Clear;

    //if FBrushList.Brush[b]^.Image.FormatInfo.BytesPerPixel = 2
    // then FillWord(FBrushList.Brush[b]^.Image.Bits^,
    //        FBrushList.Brush[b]^.Image.Width * FBrushList.Brush[b]^.Image.Height,
    //        $0000)
    // else FillDWord(FBrushList.Brush[b]^.Image.Bits^,
    //        FBrushList.Brush[b]^.Image.Width * FBrushList.Brush[b]^.Image.Height,
    //        $00000000);

    t := ResMan.Tiledata.LandTiles[FBrushList.Brush[b]^.BTile[0]^.ID].TextureID;
    texmEntry := TTexture(ResMan.Texmaps.Block[t]);
    //texmEntry.Graphic.CopyTo(0, 0, texmEntry.Graphic.Width, texmEntry.Graphic.Height,
    //        FBrushList.Brush[b]^.Image, 0, 0);
    texmEntry.Graphic.StretchTo(0, 0, texmEntry.Graphic.Width, texmEntry.Graphic.Height,
            FBrushList.Brush[b]^.Image, 2, 2,
            FBrushList.Brush[b]^.Image.Width-4, FBrushList.Brush[b]^.Image.Height-4,
            rfNearest);
    texmEntry.Free;
  end;

  {  // Отладка - проверка считанного файла
  Logger.EnterMethod([lcInfo, lcDebug], 'TfrmMain.LoadBrushTilesList');
  Logger.Send([lcInfo], 'Brushes Number: %x', [FBrushList.Count]);
  for b := 0 to FBrushList.Count - 1 do begin
    Logger.Send([lcInfo], '<Brush Id="%.4d" Name="%s" Tiles="%d" Edges="%d">',
      [FBrushList.Brush[b]^.ID, FBrushList.Brush[b]^.Name, FBrushList.Brush[b]^.Count, FBrushList.Brush[b]^.ECount]);
    for t := 0 to FBrushList.Brush[b]^.Count - 1 do
      Logger.Send([lcInfo], '    <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BTile[t]^.ID, FBrushList.Brush[b]^.BTile[t]^.Chance, FBrushList.Brush[b]^.BTile[t]^.Mask]);
    for e := 0 to FBrushList.Brush[b]^.ECount - 1 do begin
      Logger.Send([lcInfo], '    <Edge To="%d" Tiles="%d">',
        [FBrushList.Brush[b]^.EdgeId[e]^, FBrushList.Brush[b]^.BEdges[e]^.CountUU+FBrushList.Brush[b]^.BEdges[e]^.CountUR+FBrushList.Brush[b]^.BEdges[e]^.CountLL+FBrushList.Brush[b]^.BEdges[e]^.CountUL+FBrushList.Brush[b]^.BEdges[e]^.CountDL+FBrushList.Brush[b]^.BEdges[e]^.CountDR]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountUU - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileUU[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileUU[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileUU[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountUR - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileUR[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileUR[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileUR[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountLL - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileLL[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileLL[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileLL[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountUL - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileUL[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileUL[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileUL[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountDL - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileDL[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileDL[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileDL[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountDR - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileDR[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileDR[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileDR[t]^.Mask]);
    Logger.Send([lcInfo], '    </Edge>');
    end;
    Logger.Send([lcInfo], '</Brush>');
  end;
  Logger.ExitMethod([lcInfo, lcDebug], 'TfrmMain.LoadBrushTilesList');
  }
end;

procedure TfrmMain.BuildGroupList;
var
  XMLDoc: TXMLDocument;
  iNode : TDOMNode;
  fPath : string;
  vNode : PVirtualNode;
  bTemp : array[0..$E000] of Word;
  gTemp : array[0..$FFFF] of Boolean;

  procedure SumItems(Node: PVirtualNode; Count: Integer; var Items: LongWord);
  var
    i    : Integer;
    item : PVirtualNode;
    group: PGroupNode;
  begin
    group := tvGroups.GetNodeData(Node);
    Items += group^.Count;
    Items += group^.Entries;
    Items += group^.Brushes;

    for i := 0 to group^.Links - 1 do
      if group^.GLink[i] <> nil then
        SumItems(group^.GLink[i], 0, Items);

    if (Count > 0) then
    begin
      item := tvGroups.GetFirstChild(Node);
      while item <> nil do
      begin
        SumItems(item, Count - 1, Items);
        item := tvGroups.GetNextSibling(item);
      end;
    end;
  end;

  procedure ProcessNode(Node: TDOMNode; TreeNode: PVirtualNode);
  var
    element: TDOMElement;
    cNode: TDOMNode;
    s: string;
    i, j, k, e, b, d, q, id: Integer;
    group: PGroupNode;
  begin
    if Node = nil then Exit; // выходим, если достигнут конец документа
    if LowerCase(Node.NodeName) <> 'group' then Exit;

    // добавляем узел в дерево
    TreeNode := tvGroups.AddChild(TreeNode);
    group := tvGroups.GetNodeData(TreeNode);

    // сохраняем данные узла
    e := 0;  b := 0;  d := 0;
    group^.Count := 0;
    cNode := Node.FirstChild;
    while cNode <> nil do
    begin
      s := LowerCase(cNode.NodeName);
      if (s = 'tile') or (s = 'land') or (s = 'item')
         then Inc(group^.Count, 1)
         else if (s = 'entry') then Inc(e, 1)
         else if (s = 'brush') then Inc(b, 1)
         else if (s = 'link')  then // Inc(d, 1);
           for i := 0 to cNode.Attributes.Length - 1 do
             if LowerCase(cNode.Attributes[i].NodeName) = 'groupid' then Inc(d, 1)
             else if LowerCase(cNode.Attributes[i].NodeName) = 'brushid' then
               if TryStrToInt(cNode.Attributes[i].NodeValue, id) then begin
                 for j := 0 to Length(FBrushList.Tiles) - 1 do
                   if (FBrushList.Tiles[j].ID <> $FFFF) and
                      ((id = FBrushList.Tiles[j].Brush1^.ID) or (id = FBrushList.Tiles[j].Brush2^.ID))
                   then Inc(group^.Count, 1);
               end;
      cNode := cNode.NextSibling;
    end;

    group^.Entries := e;  group^.Brushes := b;  group^.Links := d;
    k := 0; e := 0; b := 0; d := 0;
    getmem(group^.lids,  group^.Links * SizeOf(LongWord));
    getmem(group^.GLink, group^.Links * SizeOf(PVirtualnode));
    getmem(group^.GTile, group^.Count * SizeOf(TGroupTile));
    getmem(group^.Entry, group^.Entries * SizeOf(PGroupEntry));
    getmem(group^.Brush, group^.Brushes * SizeOf(PGroupBrush));
    cNode := Node.FirstChild;
    while cNode <> nil do
    begin
      s := LowerCase(cNode.NodeName);
      if (s = 'tile') or (s = 'land') or (s = 'item') then
      begin
        for i := cNode.Attributes.Length - 1 downto 0 do
        begin
          if LowerCase(cNode.Attributes[i].NodeName) = 'id' then
            if TryStrToInt(cNode.Attributes[i].NodeValue, id) then
            begin
              if s = 'item' then
                Inc(id, $4000);
              group^.GTile[k].ID := id;
              Inc(k, 1);
            end;
        end;
      end
      else if (s = 'entry') then
      begin
        for i := cNode.Attributes.Length - 1 downto 0 do
        begin
          if LowerCase(cNode.Attributes[i].NodeName) = 'id' then
            if TryStrToInt(cNode.Attributes[i].NodeValue, id) then
            begin
              group^.Entry[e] := nil;
              for j := 0 to FEntryList.Count - 1 do
                if FEntryList.Entry[j]^.ID = id then begin
                  group^.Entry[e] := FEntryList.Entry[j];
                  break;
                end;
              Inc(e, 1);
            end;
        end;
      end
      else if (s = 'brush') then
      begin
        for i := cNode.Attributes.Length - 1 downto 0 do
        begin
          if LowerCase(cNode.Attributes[i].NodeName) = 'id' then
            if TryStrToInt(cNode.Attributes[i].NodeValue, id) then
            begin
              group^.Brush[b] := nil;
              for j := 0 to FBrushList.Count - 1 do
                if FBrushList.Brush[j]^.ID = id then begin
                  group^.Brush[b] := FBrushList.Brush[j];
                  break;
                end;
              Inc(b, 1);
            end;
        end;
      end
      else if (s = 'link') then
      begin
        for i := 0 to cNode.Attributes.Length - 1 do
        begin
          if LowerCase(cNode.Attributes[i].NodeName) = 'groupid' then
            if TryStrToInt(cNode.Attributes[i].NodeValue, id) then
            begin
              group^.lids[d] := id;
              Inc(d, 1);
            end;
          if LowerCase(cNode.Attributes[i].NodeName) = 'brushid' then  //
            if TryStrToInt(cNode.Attributes[i].NodeValue, id) then
            begin
               bTemp[$0000]:=$0000; bTemp[$D000]:=$D000;
               bTemp[$1000]:=$1000; bTemp[$2000]:=$2000; bTemp[$3000]:=$3000; bTemp[$4000]:=$4000;
               bTemp[$5000]:=$5000; bTemp[$6000]:=$6000; bTemp[$7000]:=$7000; bTemp[$8000]:=$8000;
               bTemp[$9000]:=$9000; bTemp[$A000]:=$A000; bTemp[$B000]:=$B000; bTemp[$C000]:=$C000;
               for j := 0 to Length(gTemp)-1 do gTemp[j] := False;
               for j := 0 to Length(FBrushList.Tiles) - 1 do begin
                 if (FBrushList.Tiles[j].ID = $FFFF) then Continue;
                 if (id = FBrushList.Tiles[j].Brush1^.ID) and (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$0000], 1); bTemp[bTemp[$0000]] := j; Continue;
                 end;

                 if (FBrushList.Tiles[j].Mask = $0B) and (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$1000],1); bTemp[bTemp[$1000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush1^.ID]:=True;
                   bTemp[bTemp[$1000]+$0800]:=FBrushList.Tiles[j].Brush1^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $0D) and (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$2000],1); bTemp[bTemp[$2000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush1^.ID]:=True;
                   bTemp[bTemp[$2000]+$0800]:=FBrushList.Tiles[j].Brush1^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $07) and (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$3000],1); bTemp[bTemp[$3000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush1^.ID]:=True;
                   bTemp[bTemp[$3000]+$0800]:=FBrushList.Tiles[j].Brush1^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $0E) and (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$4000],1); bTemp[bTemp[$4000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush1^.ID]:=True;
                   bTemp[bTemp[$4000]+$0800]:=FBrushList.Tiles[j].Brush1^.ID;
                   Continue;
                 end;

                 if (FBrushList.Tiles[j].Mask = $09) and (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$5000],1); bTemp[bTemp[$5000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush1^.ID]:=True;
                   bTemp[bTemp[$5000]+$0800]:=FBrushList.Tiles[j].Brush1^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $03) and (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$6000],1); bTemp[bTemp[$6000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush1^.ID]:=True;
                   bTemp[bTemp[$6000]+$0800]:=FBrushList.Tiles[j].Brush1^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $03) and (id = FBrushList.Tiles[j].Brush1^.ID) then begin
                   inc(bTemp[$7000],1); bTemp[bTemp[$7000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush2^.ID]:=True;
                   bTemp[bTemp[$7000]+$0800]:=FBrushList.Tiles[j].Brush2^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $09) and (id = FBrushList.Tiles[j].Brush1^.ID) then begin
                   inc(bTemp[$8000],1); bTemp[bTemp[$8000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush2^.ID]:=True;
                   bTemp[bTemp[$8000]+$0800]:=FBrushList.Tiles[j].Brush2^.ID;
                   Continue;
                 end;

                 if (FBrushList.Tiles[j].Mask = $0E) and (id = FBrushList.Tiles[j].Brush1^.ID) then begin
                   inc(bTemp[$9000],1); bTemp[bTemp[$9000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush2^.ID]:=True;
                   bTemp[bTemp[$9000]+$0800]:=FBrushList.Tiles[j].Brush2^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $0D) and (id = FBrushList.Tiles[j].Brush1^.ID) then begin
                   inc(bTemp[$A000],1); bTemp[bTemp[$A000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush2^.ID]:=True;
                   bTemp[bTemp[$A000]+$0800]:=FBrushList.Tiles[j].Brush2^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $07) and (id = FBrushList.Tiles[j].Brush1^.ID) then begin
                   inc(bTemp[$B000],1); bTemp[bTemp[$B000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush2^.ID]:=True;
                   bTemp[bTemp[$B000]+$0800]:=FBrushList.Tiles[j].Brush2^.ID;
                   Continue;
                 end;
                 if (FBrushList.Tiles[j].Mask = $0B) and (id = FBrushList.Tiles[j].Brush1^.ID) then begin
                   inc(bTemp[$C000],1); bTemp[bTemp[$C000]]:=j;
                   gTemp[FBrushList.Tiles[j].Brush2^.ID]:=True;
                   bTemp[bTemp[$C000]+$0800]:=FBrushList.Tiles[j].Brush2^.ID;
                   Continue;
                 end;

                 if (id = FBrushList.Tiles[j].Brush1^.ID) or (id = FBrushList.Tiles[j].Brush2^.ID) then begin
                   inc(bTemp[$D000],1);     // то что не удалось расортировать (вообще мы никогда не должны сюда попадать)
                   bTemp[bTemp[$D000]] := j;
                 end;

               end;

               for j := $0001 to bTemp[$0000] do begin
                 group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
               end;

               for q := 0 to Length(gTemp)-1 do
                 if gTemp[q] then begin
                   for j := $1001 to bTemp[$1000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $2001 to bTemp[$2000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $3001 to bTemp[$3000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $4001 to bTemp[$4000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $5001 to bTemp[$5000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $6001 to bTemp[$6000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $7001 to bTemp[$7000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $8001 to bTemp[$8000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $9001 to bTemp[$9000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $A001 to bTemp[$A000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $B001 to bTemp[$B000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                   for j := $C001 to bTemp[$C000] do if (bTemp[j+$0800]=q) then begin
                     group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
                   end;
                 end;

               for j := $D001 to bTemp[$D000] do begin
                 group^.GTile[k].ID := bTemp[j]; Inc(k, 1);
               end;

            end;
        end;
      end;
      cNode := cNode.NextSibling;
    end;

    // переходим к дочернему узлу
    cNode := Node.FirstChild;

    // проходим по всем дочерним узлам
    while cNode <> nil do
    begin
      ProcessNode(cNode, TreeNode);
      cNode := cNode.NextSibling;
    end;

    // сохраняем данные узла
    k:= 0;
    group^.ID   := 0;
    group^.Nodes:= 0;
    group^.Name := 'NoName';
    group^.Color:= TColor($00000000);
    if tvGroups.ChildCount[TreeNode] = 0
       then group^.Ital := True
       else group^.Ital := False;
    if tvGroups.ChildCount[TreeNode] > 4
       then group^.Bold := True
       else group^.Bold := False;
    if Node.HasAttributes and (Node.Attributes.Length>0) then
      for i := 0 to Node.Attributes.Length - 1 do
        if LowerCase(Node.Attributes[i].NodeName) = 'name' then begin
          group^.Name := CP1251ToUTF8(Node.Attributes[i].NodeValue)
        end else if LowerCase(Node.Attributes[i].NodeName) = 'color'  then begin
          if TryStrToInt(Node.Attributes[i].NodeValue, k) then
          group^.Color:=TColor(((k and$0000FF)shl 16)or(k and$00FF00)or((k and$FF0000)shr 16));
        end else if LowerCase(Node.Attributes[i].NodeName) = 'bold'   then begin
          TryStrToBool(Node.Attributes[i].NodeValue, group^.Bold);
        end else if LowerCase(Node.Attributes[i].NodeName) = 'ital'   then begin
          TryStrToBool(Node.Attributes[i].NodeValue, group^.Ital);
        end else if LowerCase(Node.Attributes[i].NodeName) = 'id'     then begin
          if TryStrToInt(Node.Attributes[i].NodeValue, d) then group^.ID := d;
        end else if LowerCase(Node.Attributes[i].NodeName) = 'nodes'  then begin
          if TryStrToInt(Node.Attributes[i].NodeValue, k) then
            begin if k=-1 then k:=$FFFF; group^.Nodes := Max(0, k); k:=0; end;
        end;
  end;

  procedure BuildLinks(Node: PVirtualNode);
  var
    i    : Integer;
    item : PVirtualNode;
    group: PGroupNode;
  begin
    group := tvGroups.GetNodeData(Node);

    for i := 0 to group^.Links - 1 do
    begin
      //Logger.Send([lcClient, lcDebug], 'BuildLinks: %.4d / %.4d - id: %d', [i+1, group^.Links, group^.lids[i]]);
      group^.GLink[i] := nil;
      if group^.lids[i] = 0 then continue;

      item := tvGroups.GetFirst();
      while item <> nil do
      begin
        if group^.lids[i] = PGroupNode(tvGroups.GetNodeData(item))^.ID then
        begin
          //Logger.Send([lcClient, lcDebug], 'BuildLinks: Group id: %d Found', [group^.lids[i]]);
          group^.GLink[i] := item;
          break;
        end;
        item := tvGroups.GetNext(item);
      end
    end;

    FreeMem(group^.lids);
    group^.lids := nil;
  end;

begin
  tvGroups.BeginUpdate;
  tvGroups.Clear;

  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['TilesGroup.xml']));
  if FileExists(FProfileDir + 'TilesGroup.xml')
    then fPath := (FProfileDir + 'TilesGroup.xml')
    else if FileExists(FLocalDir + 'TilesGroup.xml')
      then fPath := (FLocalDir + 'TilesGroup.xml')
      else if FileExists(FConfigDir + 'TilesGroup.xml')
        then fPath := (FConfigDir + 'TilesGroup.xml')
        else if FileExists(ResMan.GetFile('TilesGroup.xml'))
          then fPath := (ResMan.GetFile('TilesGroup.xml'))
          else Exit;

  // Читаем xml файл с жесткого диска
  ReadXMLFile(XMLDoc, fPath);
  if LowerCase(XMLDoc.DocumentElement.NodeName) = 'tilesgroup' then
  begin
    iNode := XMLDoc.DocumentElement.FirstChild;
    while iNode <> nil do
    begin
      if LowerCase(iNode.NodeName) = 'group' then
        ProcessNode(iNode, nil); // Рекурсия
      iNode := iNode.NextSibling;
    end;

    // Построение Указателей
    vNode := tvGroups.GetFirst();
    while vNode <> nil do
    begin
      BuildLinks(vNode);
      vNode := tvGroups.GetNext(vNode);
    end;
    // Подсчет Items
    vNode := tvGroups.GetFirst();
    while vNode <> nil do
    begin
      SumItems(vNode, PGroupNode(tvGroups.GetNodeData(vNode))^.Nodes,
                      PGroupNode(tvGroups.GetNodeData(vNode))^.Items);
      vNode := tvGroups.GetNext(vNode);
    end;
  end;

  XMLDoc.Free;
  tvGroups.EndUpdate;
end;

procedure TfrmMain.FreeGroupLists;
var i, j, k : Integer;
begin
  for i:=0 to FEntryList.Count-1 do begin
    FreeMem(FEntryList.Entry[i]^.ETile);
    FEntryList.Entry[i]^.Image.Destroy;
  end;
  FreeMem(FEntryList.Entry);
  FEntryList.Count:=0;

  for i:=0 to FBrushList.Count-1 do begin
    //for j:=0 to FBrushList.Brush[i]^.Count-1 do begin
    //  FreeMem(FBrushList.Brush[i]^.BTile[j]);
    //end;
    FreeMem(FBrushList.Brush[i]^.BEdges);
    FreeMem(FBrushList.Brush[i]^.BTile);
    FBrushList.Brush[i]^.Image.Destroy;
    FBrushList.Count:=0;
  end;

  for i:=0 to $3FFF do begin
    FBrushList.Tiles[i].ID := 0;
    FBrushList.Tiles[i].Brush1:=nil;
    FBrushList.Tiles[i].Brush2:=nil;
  end;

end;

procedure TfrmMain.BuildTileList;
var
  minID, maxID, i, k, lastID: Integer;
  item : PVirtualItem;
  groupNode : PVirtualNode;
  tileInfo: PTileInfo;
  filter  : string;
  nodeData: PGroupNode;

  procedure AddNodeTiles(Node: PVirtualNode; Count: Integer);
  var
    item: PVirtualItem;
    groupNode: PVirtualNode;
    nodeData: PGroupNode;
    tileInfo: PTileInfo;
    lastID: LongWord;
    i, j: Integer;

    //b,t,e: Integer;
  begin
    nodeData := tvGroups.GetNodeData(Node);
    Logger.Send([lcInfo], 'Brushes: %d - Entries: %d - Tiles: %d', [nodeData^.Brushes, nodeData^.Entries, nodeData^.Count]);
  {
  Logger.EnterMethod([lcInfo, lcDebug], 'TfrmMain.AddNodeTiles-Entries');
  Logger.Send([lcInfo], 'Entries Number: %d', [FEntryList.Count]);
  for e := 0 to FEntryList.Count - 1 do begin
    Logger.Send([lcInfo], '<Entry Id="%.4d" Name="%s" Tiles="%x">',
      [FEntryList.Entry[e]^.ID, FEntryList.Entry[e]^.Name, FEntryList.Entry[e]^.Count]);
    for t := 0 to FEntryList.Entry[e]^.Count - 1 do
      Logger.Send([lcInfo], '    <Tile Id="0x%.5x" Hue="0x%.3x" X="%d" Y="%d" Z="%d">',
        [FEntryList.Entry[e]^.ETile[t].ID, FEntryList.Entry[e]^.ETile[t].Hue,
        FEntryList.Entry[e]^.ETile[t].X, FEntryList.Entry[e]^.ETile[t].Y, FEntryList.Entry[e]^.ETile[t].Z]);
    Logger.Send([lcInfo], '</Entry>');
  end;
  Logger.ExitMethod([lcInfo, lcDebug], 'TfrmMain.AddNodeTiles-Entries');
  Logger.EnterMethod([lcInfo, lcDebug], 'TfrmMain.AddNodeTiles-Brushes');
  Logger.Send([lcInfo], 'Brushes Number: %x', [FBrushList.Count]);
  for b := 0 to FBrushList.Count - 1 do begin
    Logger.Send([lcInfo], '<Brush Id="%.4d" Name="%s" Tiles="%d" Edges="%d">',
      [FBrushList.Brush[b]^.ID, FBrushList.Brush[b]^.Name, FBrushList.Brush[b]^.Count, FBrushList.Brush[b]^.ECount]);
    for t := 0 to FBrushList.Brush[b]^.Count - 1 do
      Logger.Send([lcInfo], '    <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BTile[t]^.ID, FBrushList.Brush[b]^.BTile[t]^.Chance, FBrushList.Brush[b]^.BTile[t]^.Mask]);
    for e := 0 to FBrushList.Brush[b]^.ECount - 1 do begin
      Logger.Send([lcInfo], '    <Edge To="%d" Tiles="%d">',
        [FBrushList.Brush[b]^.EdgeId[e]^, FBrushList.Brush[b]^.BEdges[e]^.CountUU+FBrushList.Brush[b]^.BEdges[e]^.CountUR+FBrushList.Brush[b]^.BEdges[e]^.CountLL+FBrushList.Brush[b]^.BEdges[e]^.CountUL+FBrushList.Brush[b]^.BEdges[e]^.CountDL+FBrushList.Brush[b]^.BEdges[e]^.CountDR]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountUU - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileUU[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileUU[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileUU[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountUR - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileUR[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileUR[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileUR[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountLL - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileLL[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileLL[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileLL[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountUL - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileUL[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileUL[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileUL[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountDL - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileDL[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileDL[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileDL[t]^.Mask]);
    for t := 0 to FBrushList.Brush[b]^.BEdges[e]^.CountDR - 1 do
      Logger.Send([lcInfo], '        <Land Id="0x%.4x" Chance="%.2f" Mask="0x%.2x">',
        [FBrushList.Brush[b]^.BEdges[e]^.BTileDR[t]^.ID, FBrushList.Brush[b]^.BEdges[e]^.BTileDR[t]^.Chance, FBrushList.Brush[b]^.BEdges[e]^.BTileDR[t]^.Mask]);
    Logger.Send([lcInfo], '    </Edge>');
    end;
    Logger.Send([lcInfo], '</Brush>');
  end;
  Logger.ExitMethod([lcInfo, lcDebug], 'TfrmMain.AddNodeTiles-Brushes');
  }

    for i := 0 to nodeData^.Count - 1 do
    begin
      lastID := nodeData^.GTile[i].ID;
      if FVisibleTiles[lastID] and ResMan.Art.Exists(lastID) then
      begin
        if (filter <> '') and (Pos(filter, AnsiLowerCase(
          ResMan.Tiledata.TileData[lastID].TileName)) = 0) then Continue;
        item := vdtTiles.AddItem(nil);
        tileInfo := vdtTiles.GetNodeData(item);
        tileInfo^.ID := lastID;
      end;
    end;

    for i := 0 to nodeData^.Brushes - 1 do
    begin
      if nodeData^.Brush[i] = nil then Continue;
      lastID := $2F000000 + LongWord(nodeData^.Brush[i]^.ID);
      //if FVisibleTiles[lastID] and ResMan.Art.Exists(lastID) then
      begin
        if (filter <> '') and (Pos(filter, AnsiLowerCase(
            nodeData^.Brush[i]^.Name)) = 0) then Continue;
        item := vdtTiles.AddItem(nil);
        tileInfo := vdtTiles.GetNodeData(item);
        tileInfo^.ID := lastID;
        tileInfo^.ptr:= nodeData^.Brush[i];
      end;
    end;

    for i := 0 to nodeData^.Entries - 1 do
    begin
      if nodeData^.Entry[i] = nil then Continue;
      lastID := $1F000000 + LongWord(nodeData^.Entry[i]^.ID);
      //if FVisibleTiles[lastID] and ResMan.Art.Exists(lastID) then
      begin
        if (filter <> '') and (Pos(filter, AnsiLowerCase(
            nodeData^.Entry[i]^.Name)) = 0) then Continue;
        item := vdtTiles.AddItem(nil);
        tileInfo := vdtTiles.GetNodeData(item);
        tileInfo^.ID := lastID;
        tileInfo^.ptr:= nodeData^.Entry[i];
      end;
    end;

    // Добавление сылок групп
    for i := 0 to nodeData^.Links - 1 do
      if nodeData^.GLink[i] <> nil then
        AddNodeTiles(nodeData^.GLink[i], 0);

    // Добавление вложенных подгрупп
    if (Count > 0) then
    begin
      groupNode := tvGroups.GetFirstChild(Node);
      while groupNode <> nil do
      begin
        AddNodeTiles(groupNode, Count - 1);
        groupNode := tvGroups.GetNextSibling(groupNode);
      end;
    end;

  end;

begin
  filter := AnsiLowerCase(UTF8ToCP1251(edFilter.Text));

  Logger.Send([lcInfo], 'TfrmMain.BuildTileList: %s', ['Start']);
  // Сортировка по группам
  if (not cbStatics.Checked) and (not cbTerrain.Checked) then
  begin
    vdtTiles.BeginUpdate;
    vdtTiles.Clear;
    groupNode := tvGroups.GetFirstSelected();
    while groupNode <> nil do
    begin
      nodeData := tvGroups.GetNodeData(groupNode);
      AddNodeTiles(groupNode, nodeData^.Nodes);
      groupNode := tvGroups.GetNextSelected(groupNode);
    end;
    vdtTiles.EndUpdate;
  end else
  // Старое построение - список всех land и\или item тайлов
  begin
    Logger.Send([lcInfo], 'TfrmMain.BuildTileList: start');
    maxID := $3FFF;
    if cbTerrain.Checked then minID := $0 else minID := $4000;
    if cbStatics.Checked then maxID := maxID + FLandscape.MaxStaticID;

    item := vdtTiles.GetFirstSelected;
    if item <> nil then
    begin
      tileInfo := vdtTiles.GetNodeData(item);
      lastID := tileInfo^.ID;
    end else
      lastID := -1;
  
    vdtTiles.BeginUpdate;
    vdtTiles.Clear;

    Logger.Send([lcInfo], 'TfrmMain.BuildTileList: from %.4x to %.4x', [minID, maxID]);
    for i := minID to maxID do
    begin
      if FVisibleTiles[i] and ResMan.Art.Exists(i) then
      begin
        if (filter <> '') and (Pos(filter, AnsiLowerCase(
          ResMan.Tiledata.TileData[i].TileName)) = 0) then Continue;
        item := vdtTiles.AddItem(nil);
        tileInfo := vdtTiles.GetNodeData(item);
        tileInfo^.ID := i;

        //Logger.Send([lcInfo], 'TfrmMain.BuildTileList: tileInfo %.4x == %.4x', [i, PTileInfo(vdtTiles.GetNodeData(vdtTiles.GetFirst()))^.ID]);

        if i = lastID then
          vdtTiles.Selected[item] := True;
      end;
    end;
  
    vdtTiles.EndUpdate;
  
    item := vdtTiles.GetFirstSelected;
    if item <> nil then
      vdtTiles.FocusedNode := item;
  end;

  Logger.Send([lcInfo], 'TfrmMain.BuildTileList: %s', ['End']);
end;

procedure TfrmMain.ProcessToolState;
var
  blockInfo: PBlockInfo;
begin
  if acSelect.Checked then
  begin
    //lblTip.Caption := 'Нажатие правой кнопки мышки покажет меню со всеми инструментами.';
    //lblTip.Caption := 'Нажмите и удерживайте левую кнопку мышки, чтобы просмотреть список действий.';
    //'Press and hold the left mouse button to show a list with actions (eg. grab hue).';
    oglGameWindow.Cursor := +01;//crDefault;

    //no highlighted tiles in "selection" mode
    Logger.Send([lcClient, lcDebug], 'Disable highlighting');
    blockInfo := nil;
    while FScreenBuffer.Iterate(blockInfo) do
      if blockInfo^.State = ssNormal then
        blockInfo^.Highlighted := False;
  end else
  begin
    //lblTip.Caption := 'Нажмите и удерживайте левую кнопку мышки, чтобы выбрать область.';
    //'Press and hold the left mouse button to target an area.';
    oglGameWindow.Cursor := +03;//crHandPoint;
  end;

  FRepaintNeeded := True;
end;

procedure TfrmMain.ProcessAccessLevel;
begin
  mnuAdministration.Visible := (dmNetwork.AccessLevel >= alDeveloper);
  mnuShutdown.Visible := (dmNetwork.AccessLevel >= alAdministrator);
  mnuAccountControl.Visible := (dmNetwork.AccessLevel >= alAdministrator);
  mnuRegionControl.Visible := (dmNetwork.AccessLevel >= alAdministrator);
  acSelection.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acDraw.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acMove.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acElevate.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acSurfElevate.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acSurfStretch.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acSurfSmooth.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acDelete.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acHue.Enabled := (dmNetwork.AccessLevel >= alNormal);
  acFill.Enabled := (dmNetwork.AccessLevel >= alNormal);
  Caption := Format('UO CentrED+ v%s    [%s "%s" (%s)  | %s "%s"]',
  //Caption := Format('UO CentrED+ v%s !!! pre-release (not stable version) !!!    [%s "%s" (%s)  | %s "%s"]',
    [VersionInfo.GetProductVersionString, lbFormTitleAccount, dmNetwork.Username,
    GetAccessLevel(dmNetwork.AccessLevel), lbFormTitleProfile, dmNetwork.Profile]);
end;

procedure TfrmMain.RebuildScreenBuffer;
var
  blockInfo: PBlockInfo;
  i, tileX, tileY: Integer;
  virtualTile: TVirtualTile;
  zoom: Single;
begin
  //Logger.EnterMethod([lcClient], 'RebuildScreenBuffer');
  if tbZoom.Down then zoom:= tbZoom.Tag / 1000.0 else zoom:= 1.0;

  FDrawDistance := Trunc(Sqrt(oglGameWindow.Width * oglGameWindow.Width +
    oglGamewindow.Height * oglGamewindow.Height) / (44 * zoom));
  //Logger.Send([lcClient], 'DrawDistance', FDrawDistance);

  {$HINTS off}{$WARNINGS off}
  if FX - FDrawDistance < 0 then FLowOffsetX := -FX else FLowOffsetX := -FDrawDistance;
  if FY - FDrawDistance < 0 then FLowOffsetY := -FY else FLowOffsetY := -FDrawDistance;
  if FX + FDrawDistance >= FLandscape.Width * 8 then FHighOffsetX := FLandscape.Width * 8 - FX - 1 else FHighOffsetX := FDrawDistance;
  if FY + FDrawDistance >= FLandscape.Height * 8 then FHighOffsetY := FLandscape.Height * 8 - FY - 1 else FHighOffsetY := FDrawDistance;
  {$HINTS on}{$WARNINGS on}

  FRangeX := FHighOffsetX - FLowOffsetX;
  FRangeY := FHighOffsetY - FLowOffsetY;

  FLandscape.PrepareBlocks((FX + FLowOffsetX) div 8, (FY + FLowOffsetY) div 8,
    (FX + FHighOffsetX) div 8 + 1, (FY + FHighOffsetY) div 8 + 1);

  if frmVirtualLayer.cbShowLayer.Checked then
  begin
    //Logger.Send([lcClient, lcDebug], 'Preparing Virtual Layer');

    if FVLayerMaterial = nil then
      FVLayerMaterial := TSimpleMaterial.Create(FVLayerImage);

    i := 0;
    for tileX := FX + FLowOffsetX to FX + FHighOffsetX do
    begin
      for tileY := FY + FLowOffsetY to FY + FHighOffsetY do
      begin
        while (i < FVirtualTiles.Count) and (not (FVirtualTiles[i] is TVirtualTile)) do
          Inc(i);

        if i < FVirtualTiles.Count then
        begin
          virtualTile := TVirtualTile(FVirtualTiles[i]);
        end else
        begin
          virtualTile := TVirtualTile.Create(nil);
          FVirtualTiles.Add(virtualTile);
        end;

        virtualTile.X := tileX;
        virtualTile.Y := tileY;
        virtualTile.Z := frmVirtualLayer.seZ.Value;
        virtualTile.Priority := virtualTile.Z;
        virtualTile.PriorityBonus := High(ShortInt);

        Inc(i);
      end;
    end;
    while i < FVirtualTiles.Count do
    begin
      if FVirtualTiles[i] is TVirtualTile then
        FVirtualTiles.Delete(i)
      else
        Inc(i);
    end;
  end else
  begin
    for i := FVirtualTiles.Count - 1 downto 0 do
      if FVirtualTiles[i] is TVirtualTile then
        FVirtualTiles.Delete(i);
  end;

  if acNoDraw.Checked and mnuShowLightSource.Checked then
  begin
    if FVLightSrcMaterial = nil then begin
      getmem(FVLightSrcMaterial, FVLightSrcImageCount * SizeOf(TSimpleMaterial));
      for i := 1 to FVLightSrcImageCount do
          FVLightSrcMaterial[i-1] := TSimpleMaterial.Create(FVLightSrcImage[i]);
    end;
  end;

  //Logger.Send([lcClient, lcDebug], 'VirtualTiles', FVirtualTiles.Count);

  FLandscape.FillDrawList(FScreenBuffer, FX + FLowOffsetX, FY + FLowOffsetY,
    FRangeX, FRangeY, acTerrain.Checked, acStatics.Checked, mnuShowWalls.Checked,
    mnuShowBridges.Checked, mnuShowRoofs.Checked, mnuShowSurfaces.Checked, mnuShowFoliage.Checked,
    mnuShowWater.Checked, acNoDraw.Checked and mnuShowNoDrawTiles.Checked, FVirtualTiles);

  //Pre-process the buffer
  blockInfo := nil;
  while FScreenBuffer.Iterate(blockInfo) do
    PrepareScreenBlock(blockInfo);

  FScreenBuffer.UpdateShortcuts;
  FScreenBufferState := [sbsValid, sbsIndexed];

  //Logger.ExitMethod([lcClient], 'RebuildScreenBuffer');
end;

procedure TfrmMain.UpdateCurrentTile;
var
  localPos: TPoint;
begin
  if oglGameWindow.MouseEntered then
  begin
    localPos := oglGameWindow.ScreenToClient(Mouse.CursorPos);
    UpdateCurrentTile(localPos.X, localPos.Y);
  end;
end;

procedure TfrmMain.UpdateCurrentTile(AX, AY: Integer);
var
  blockInfo: PBlockInfo;
  zoom: Single;
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'UpdateCurrentTile');
  FOverlayUI.ActiveArrow := FOverlayUI.HitTest(AX, AY);
  if FOverlayUI.ActiveArrow > -1 then
  begin
    //Logger.Send([lcClient, lcDebug], 'Overlay active');
    CurrentTile := nil;
    //Logger.ExitMethod([lcClient, lcDebug], 'UpdateCurrentTile');
    Exit;
  end;

  if tbZoom.Down then zoom := tbZoom.Tag / 1000.0 else zoom := 1.0;

  blockInfo := FScreenBuffer.Find(Point(AX, AY), zoom);
  if blockInfo <> nil then
    CurrentTile := blockInfo^.Item
  else
    CurrentTile := nil;

  //Logger.ExitMethod([lcClient, lcDebug], 'UpdateCurrentTile');
end;

procedure TfrmMain.UpdateFilter;
var
  blockInfo: PBlockInfo;
  tileData: TTiledata;
  staticTileData: TStaticTileData;
  lastSurface: PBlockInfo;
  surfaceTop: Integer;
  zoom: Single;
begin
  blockInfo := nil;
  lastSurface := nil;
  while FScreenBuffer.Iterate(blockInfo) do
  begin
    if blockInfo^.State in [ssNormal, ssFiltered] then
    begin
      blockInfo^.State := ssNormal;
      if (blockInfo^.Item.X < frmBoundaries.seMinX.Value) or
         (blockInfo^.Item.X > frmBoundaries.seMaxX.Value) or
         (blockInfo^.Item.Y < frmBoundaries.seMinY.Value) or
         (blockInfo^.Item.Y > frmBoundaries.seMaxY.Value) or
         (blockInfo^.Item.Z < frmBoundaries.tbMinZ.Position) or
         (blockInfo^.Item.Z > frmBoundaries.tbMaxZ.Position) then
      begin
        blockInfo^.State := ssFiltered;
      end else
      if tbFilter.Down and (blockInfo^.Item is TStaticItem) and
        (not frmFilter.Filter(TStaticItem(blockInfo^.Item))) then
      begin
        blockInfo^.State := ssFiltered;
      end;

      blockInfo^.WalkRestriction := wrNone;
      if acWalkable.Checked then
      begin
        if blockInfo^.Item is TMapCell then
        begin
          tileData := ResMan.Tiledata.LandTiles[blockInfo^.Item.TileID];
          if tdfImpassable in tileData.Flags then
          begin
            blockInfo^.WalkRestriction := wrCannotWalk;
            lastSurface := nil;
          end else
          begin
            blockInfo^.WalkRestriction := wrCanWalk;
            lastSurface := blockInfo;
            surfaceTop := blockInfo^.Item.Z;
          end;
        end else
        begin
          staticTileData := ResMan.Tiledata.StaticTiles[blockInfo^.Item.TileID];
          if (lastSurface <> nil) and (lastSurface^.WalkRestriction = wrCanWalk) and
             (lastSurface^.Item.X = blockInfo^.Item.X) and
             (lastSurface^.Item.Y = blockInfo^.Item.Y) and ([tdfSurface,
              tdfImpassable] * staticTileData.Flags <> []) then
          begin
            if (blockInfo^.Item.Z < surfaceTop + 16) and
               ((blockInfo^.Item.Z > lastSurface^.Item.Z + 2) or
                not (tdfSurface in staticTileData.Flags)) then
              lastSurface^.WalkRestriction := wrCannotWalk;
          end;

          if tdfSurface in staticTileData.Flags then
          begin
            if tdfImpassable in staticTileData.Flags then
            begin
              blockInfo^.WalkRestriction := wrCannotWalk;
              lastSurface := nil;
            end else
            begin
              blockInfo^.WalkRestriction := wrCanWalk;
              lastSurface := blockInfo;
              surfaceTop := blockInfo^.Item.Z + staticTileData.Height;
            end;
          end;
        end;
      end; //acWalkable.Checked

    end;
  end;

  Include(FScreenBufferState, sbsFiltered);

  if tbZoom.Down then zoom := tbZoom.Tag / 1000.0 else zoom := 1.0;
  if (FLightManager.LightLevel > 0) and not acFlat.Checked then
    FLightManager.UpdateLightMap(FX + FLowOffsetX, FRangeX + 1, FY + FLowOffsetY,
      FRangeY + 1, FScreenBuffer, zoom);
end;

procedure TfrmMain.UpdateSelection;

  procedure SetHighlight(ABlockInfo: PBlockInfo; AHighlighted: Boolean);
  begin
    if (ABlockInfo^.Item is TStaticItem) and acHue.Checked then
    begin
      if ABlockInfo^.HueOverride <> AHighlighted then
      begin
        ABlockInfo^.HueOverride := AHighlighted;
        if AHighlighted then
        begin
          ABlockInfo^.Hue := frmHueSettings.GetHue;
          ABlockInfo^.LowRes := FTextureManager.GetStaticMaterial(
            TStaticItem(ABlockInfo^.Item), ABlockInfo^.Hue);
        end else
        begin
          ABlockInfo^.LowRes := FTextureManager.GetStaticMaterial(
            TStaticItem(ABlockInfo^.Item));
      end;
      end;
    end else
    begin
      ABlockInfo^.Highlighted := AHighlighted;
    end;
  end;

  procedure AddGhostTile(AX, AY: Word; ABaseTile: TWorldItem; selecetion: TRect);

    // Получение ID текстуры для тайлов при использовании кисти
    function GetTileId(brush: PGroupBrush; mask: byte; tileId: LongWord) : Integer;
    var
      i, j : Integer;
      tileMask : Byte;
      tileBrush, tempBrush: PGroupBrush;
      brushEdge: PGroupBrushEdge;
      randnf, chance, factor: Float;
      brushCount: LongWord;
      brushTiles: ^PBrushTile;
    begin
      GetTileId := tileId;
      if FBrushList.Tiles[tileId].ID <> tileId
      then exit;

      if (FBrushList.Tiles[tileId].Brush1^.ID <> brush^.ID) and (FBrushList.Tiles[tileId].Brush2^.ID <> brush^.ID)
        and (FBrushList.Tiles[tileId].Brush1^.ID <> FBrushList.Tiles[tileId].Brush2^.ID)
      then begin
        i:=0; j:=0;
        tileMask := FBrushList.Tiles[tileId].Mask and not(mask);
        if (tileMask and $01) <> 0 then inc(i);
        if (tileMask and $02) <> 0 then inc(i);
        if (tileMask and $04) <> 0 then inc(i);
        if (tileMask and $08) <> 0 then inc(i);
        tileMask := not(FBrushList.Tiles[tileId].Mask or $F0) and not(mask);
        if (tileMask and $01) <> 0 then inc(j);
        if (tileMask and $02) <> 0 then inc(j);
        if (tileMask and $04) <> 0 then inc(j);
        if (tileMask and $08) <> 0 then inc(j);
        if i > j
           then tileBrush := FBrushList.Tiles[tileId].Brush1
           else tileBrush := FBrushList.Tiles[tileId].Brush2;
        tileMask := $00;
      end else
      if (FBrushList.Tiles[tileId].Brush1^.ID = brush^.ID) and (FBrushList.Tiles[tileId].Brush2^.ID = brush^.ID)
      then begin
        tileBrush := nil;
      end else if (FBrushList.Tiles[tileId].Brush1^.ID = FBrushList.Tiles[tileId].Brush2^.ID)
      then begin
        tileBrush := FBrushList.Tiles[tileId].Brush1;
        tileMask  := $00;
      end else begin
        if brush^.ID = FBrushList.Tiles[tileId].Brush1^.ID then begin
          tileBrush := FBrushList.Tiles[tileId].Brush2;
          tileMask  := FBrushList.Tiles[tileId].Mask;
        end else
        if brush^.ID = FBrushList.Tiles[tileId].Brush2^.ID then begin
          tileBrush := FBrushList.Tiles[tileId].Brush1;
          tileMask  := not(FBrushList.Tiles[tileId].Mask or $F0);
        end;
      end;

      // Сумирование масок
      if tileBrush = nil then begin
        mask := $0F;
      end else begin
        //Logger.Send([lcInfo], '1 Brush Id= %d  Mask= 0x%.2x  TileMask= 0x%.2x  tileId= 0x%.4x', [brush^.ID, mask, tileMask, tileId]);
        mask := tileMask or mask;
        //Logger.Send([lcInfo], '2 Brush Id= %d  Mask= 0x%.2x  TileMask= 0x%.2x  tileId= 0x%.4x', [brush^.ID, mask, tileMask, tileId]);
        if (mask = $01) or (mask = $02) or (mask = $04) or (mask = $05) or
           (mask = $06) or (mask = $08) or (mask = $0A) or (mask = $0C)
        then begin
          mask := not(mask or $F0);
          tempBrush:= brush;
          brush:= tileBrush;
          tileBrush:= tempBrush;
        end;
      end;

      // Получение данных кисти
      brushCount:= 0;  brushEdge:= nil;  brushTiles:= nil;
      if mask = $0F then begin
        brushCount := brush^.Count;        brushTiles := brush^.BTile;
      end else begin
        for i := 0 to brush^.ECount-1 do begin
         // Logger.Send([lcInfo], 'i= %d/%d  EdgeId= %d  BrushId= %d', [i, brush^.ECount-1, mask, brush^.EdgeId[i]^, tileBrush^.ID]);
          if brush^.EdgeId[i]^ = tileBrush^.ID then begin
            brushEdge := brush^.BEdges[i];
            break;
          end; end;
        if brushEdge <> nil then
          case mask of
            $0E: begin brushCount := brushEdge^.CountDR;  brushTiles := brushEdge^.BTileDR; end;
            $0D: begin brushCount := brushEdge^.CountDL;  brushTiles := brushEdge^.BTileDL; end;
            $0B: begin brushCount := brushEdge^.CountUL;  brushTiles := brushEdge^.BTileUL; end;
            $07: begin brushCount := brushEdge^.CountUR;  brushTiles := brushEdge^.BTileUR; end;
            $09: begin brushCount := brushEdge^.CountLL;  brushTiles := brushEdge^.BTileLL; end;
            $03: begin brushCount := brushEdge^.CountUU;  brushTiles := brushEdge^.BTileUU; end;
          end;
      end;

      //Logger.Send([lcInfo], 'Brush Id= %d  Mask= 0x%.2x   TileMask= 0x%.2x  tileId= 0x%.4x', [brush^.ID, mask, tileMask, tileId]);

      // Находение ID тайла
      chance := 0; factor := 0;
      for i := 0 to brushCount - 1 do
        chance := chance + brushTiles[i]^.Chance;
      if chance > 0 then
        factor := 1.0 / chance;
      randnf := Random; chance := 0;
      for i := 0 to brushCount - 1 do begin
        chance := chance + factor * brushTiles[i]^.Chance;
        if randnf <= chance then begin
          GetTileId := brushTiles[i]^.ID;
          break;
        end;
      end;

      //if (FBrushList.Tiles[GetTileId].Mask <> FBrushList.Tiles[tileId].Mask)
      //or (FBrushList.Tiles[GetTileId].ID <> GetTileId) or (FBrushList.Tiles[tileId].ID <> tileId)
      //or (FBrushList.Tiles[GetTileId].Brush1^.ID <> FBrushList.Tiles[tileId].Brush1^.ID)
      //or (FBrushList.Tiles[GetTileId].Brush2^.ID <> FBrushList.Tiles[tileId].Brush2^.ID)
      //or (GetTileId < 0) or (GetTileId > $3FFF) then GetTileId:= -1;
    end;

  var
    blockInfo: PBlockInfo;
    tileInfo: PTileInfo;
    item: PVirtualItem;
    node: PVirtualItem;
    cell: TMapCell;
    ghostTile: TGhostTile;
    i, randalt: Integer;
  begin
    if frmDrawSettings.cbProbability.Checked and frmDrawSettings.cbProbability.Enabled
      and (frmDrawSettings.seProbability.Value < 100 * Random)
      then exit;

    tileInfo := nil;
    if frmDrawSettings.rbTileList.Checked then
    begin
      item := vdtTiles.GetFirstSelected;
      if item <> nil then
        tileInfo := vdtTiles.GetNodeData(item);
    end else if frmDrawSettings.rbRandom.Checked then
    begin
      node := vdlRandom.GetFirst;
      for i := 1 to Random(vdlRandom.TilesCount) do
        node := vdlRandom.GetNext(node);

      if node <> nil then
        tileInfo := vdlRandom.GetNodeData(node);
    end;

    if tileInfo <> nil then
    begin
      if tileInfo^.ID > $2F000000 then
      begin                                                // **** Кисти ****
        cell := FLandscape.MapCell[AX, AY];
        if cell <> nil then
        begin
          //Logger.Send([lcInfo], '!!! AX= %d  AY= %d ', [AX, AY]);
          //Logger.Send([lcInfo], 'MapCell GhostId= 0x%.4x  TileId= 0x%.4x  Id= 0x%.4x', [cell.RawTileID, cell.TileID, cell.ID]);
          inc(selecetion.Left, -1);  inc(selecetion.Top, -1);
          if not IsInRect(AX, AY, selecetion) then exit;
          inc(selecetion.Left, +1);  inc(selecetion.Top, +1);

          if (AX = selecetion.Left) then begin
            AddGhostTile(selecetion.Left-1,  AY,                  ABaseTile, selecetion);
          end;
          if (AY = selecetion.Top) then begin
            AddGhostTile(AX,                 selecetion.Top-1,    ABaseTile, selecetion);
          end;
          if (AX = selecetion.Left) and (AY = selecetion.Top) then begin
            AddGhostTile(selecetion.Left-1,  selecetion.Top-1,    ABaseTile, selecetion);
          end;

          if (AX = selecetion.Right) and (AY = selecetion.Bottom)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $01, cell.RawTileID)
          else if (AX = selecetion.Left-1) and (AY = selecetion.Bottom)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $02, cell.RawTileID)
          else if (AX = selecetion.Left-1) and (AY = selecetion.Top-1)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $04, cell.RawTileID)
          else if (AX = selecetion.Right)  and (AY = selecetion.Top-1)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $08, cell.RawTileID)
          else if (AX = selecetion.Right)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $09, cell.RawTileID)
          else if (AY = selecetion.Bottom)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $03, cell.RawTileID)
          else if (AY = selecetion.Top-1)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $0C, cell.RawTileID)
          else if (AX = selecetion.Left-1)
          then randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $06, cell.RawTileID)
          else randalt := GetTileId(PGroupBrush(tileInfo^.ptr), $0F, cell.RawTileID);

          if (randalt > 0) then begin //Добавление текстур
            cell.IsGhost := True;
            cell.GhostID := randalt;
            if frmDrawSettings.cbForceAltitude.Checked then
              cell.GhostZ := frmDrawSettings.seForceAltitude.Value
            else
              cell.GhostZ := cell.RawZ;
            if frmDrawSettings.cbRandomHeight.Checked then
              cell.GhostZ := cell.GhostZ + Random(frmDrawSettings.seRandomHeight.Value);

            PrepareMapCell(cell);
          end;
        end;
      end else if tileInfo^.ID > $1F000000 then
      begin                                                // **** Объекты *****
        for i := 0 to PGroupEntry(tileInfo^.ptr)^.Count - 1 do begin
          if frmDrawSettings.cbUseFreeTilesOnly.Checked then begin
            blockInfo := FScreenBuffer.Find(AX + PGroupEntry(tileInfo^.ptr)^.ETile[i].X,
                                            AY + PGroupEntry(tileInfo^.ptr)^.ETile[i].Y);
            if (blockInfo <> nil) and (blockInfo^.Next <> nil) and
              (blockInfo^.Item.X = blockInfo^.Next^.Item.X) and (blockInfo^.Item.Y = blockInfo^.Next^.Item.Y)
              then exit;
          end;
        end;

        if frmDrawSettings.cbRandomHeight.Checked
          then randalt := Random(frmDrawSettings.seRandomHeight.Value)
          else randalt := 0;
        for i := 0 to PGroupEntry(tileInfo^.ptr)^.Count - 1 do begin
          ghostTile := TGhostTile.Create(nil, nil, 0, 0);
          ghostTile.TileID := PGroupEntry(tileInfo^.ptr)^.ETile[i].ID - $4000;
          ghostTile.Hue := PGroupEntry(tileInfo^.ptr)^.ETile[i].Hue;
          ghostTile.X := AX + PGroupEntry(tileInfo^.ptr)^.ETile[i].X;
          ghostTile.Y := AY + PGroupEntry(tileInfo^.ptr)^.ETile[i].Y;
          ghostTile.CenterX := AX;
          ghostTile.CenterY := AY;
          if ((not frmDrawSettings.cbForceAltitude.Enabled) or (not frmDrawSettings.cbForceAltitude.Checked)) then
          begin
            if frmDrawSettings.cbUseSurfaceAltitude.Checked then begin
              blockInfo := FScreenBuffer.Find(AX, AY);
              if blockInfo <> nil then
                 ABaseTile := blockInfo^.Item;
              ghostTile.Z := ABaseTile.Z;
            end;

            if ABaseTile is TStaticItem then
              ghostTile.Z := ABaseTile.Z + ResMan.Tiledata.StaticTiles[ABaseTile.TileID].Height
            else if ABaseTile is TMapCell then
              ghostTile.Z := ResMan.Landscape.GetEffectiveAltitude(TMapCell(ABaseTile))
            else // if ABaseTile is TVirtualTile then
              ghostTile.Z := ABaseTile.Z;
          end else
            ghostTile.Z := frmDrawSettings.seForceAltitude.Value;
          ghostTile.Z := ghostTile.Z + randalt + PGroupEntry(tileInfo^.ptr)^.ETile[i].Z;;
          {
          Logger.Send([lcInfo], '    <Entry Id="%.4d  Tile %.2d/%.2d  -  Id="0x%.4x" Hue="0x%.3x" X="%d" Y="%d" Z="%d">',
            [PGroupEntry(tileInfo^.ptr)^.ID, i, PGroupEntry(tileInfo^.ptr)^.Count, ghostTile.TileID,
            ghostTile.Hue, ghostTile.X, ghostTile.Y, ghostTile.Z]);
          }
          ghostTile.UpdatePriorities(ResMan.Tiledata.StaticTiles[ghostTile.TileID], MaxInt);
          ghostTile.CanBeEdited := True;

          FVirtualTiles.Add(ghostTile);
          blockInfo := FScreenBuffer.Insert(ghostTile);
          blockInfo^.State := ssGhost;
          PrepareScreenBlock(blockInfo);
        end;
      end else if tileInfo^.ID < $4000 then
      begin                                                // **** Текстуры ****
        cell := FLandscape.MapCell[AX, AY];
        if cell <> nil then
        begin
          cell.IsGhost := True;
          cell.GhostID := tileInfo^.ID;
          if frmDrawSettings.cbForceAltitude.Checked then
            cell.GhostZ := frmDrawSettings.seForceAltitude.Value
          else
            cell.GhostZ := cell.RawZ;
          if frmDrawSettings.cbRandomHeight.Checked then
            cell.GhostZ := cell.GhostZ + Random(frmDrawSettings.seRandomHeight.Value);

          PrepareMapCell(cell);
        end;
      end else
      begin                                                // **** Статика *****
        if frmDrawSettings.cbUseFreeTilesOnly.Checked then begin
          blockInfo := FScreenBuffer.Find(AX, AY);
          if (blockInfo <> nil) and (blockInfo^.Next <> nil) and
            (blockInfo^.Item.X = blockInfo^.Next^.Item.X) and (blockInfo^.Item.Y = blockInfo^.Next^.Item.Y)
            then exit;
        end;

        ghostTile := TGhostTile.Create(nil, nil, 0, 0);
        ghostTile.TileID := tileInfo^.ID - $4000;
        ghostTile.Hue := frmHueSettings.GetHue;
        ghostTile.X := AX;    ghostTile.CenterX := AX;
        ghostTile.Y := AY;    ghostTile.CenterY := AY;

        if ((not frmDrawSettings.cbForceAltitude.Enabled) or (not frmDrawSettings.cbForceAltitude.Checked)) then
        begin
          if frmDrawSettings.cbUseSurfaceAltitude.Checked then begin
            blockInfo := FScreenBuffer.Find(AX, AY);
            if blockInfo <> nil then
              ABaseTile := blockInfo^.Item;
          end;
          //ghostTile.Z := ABaseTile.Z;
          if ABaseTile is TStaticItem then
            ghostTile.Z := ABaseTile.Z + ResMan.Tiledata.StaticTiles[ABaseTile.TileID].Height
          else if ABaseTile is TMapCell then
            ghostTile.Z := ResMan.Landscape.GetEffectiveAltitude(TMapCell(ABaseTile))
          else // if ABaseTile is TVirtualTile then
            ghostTile.Z := ABaseTile.Z;
        end else
          ghostTile.Z := frmDrawSettings.seForceAltitude.Value;
        if frmDrawSettings.cbRandomHeight.Checked then
          ghostTile.Z := ghostTile.Z +
            Random(frmDrawSettings.seRandomHeight.Value);

        ghostTile.UpdatePriorities(ResMan.Tiledata.StaticTiles[ghostTile.TileID],
          MaxInt);
        ghostTile.CanBeEdited := True;

        FVirtualTiles.Add(ghostTile);
        blockInfo := FScreenBuffer.Insert(ghostTile);
        blockInfo^.State := ssGhost;
        PrepareScreenBlock(blockInfo);
      end;

    end;
  end;

var
  selectedRect: TRect;
  blockInfo: PBlockInfo;
  item: TWorldItem;
  cell: TMapCell;
  i, tileX, tileY: Integer;
  brushMod: Boolean; // Актевирован Режим работы с кистями
begin
  //Logger.EnterMethod([lcClient, lcDebug], 'UpdateSelection');

  //If the current tile is nil, but we still have a selected tile, the
  //procedure is pointless - the selection should stay intact.
  if (CurrentTile <> nil) or (SelectedTile = nil) then
  begin
    brushMod := (vdtTiles.GetFirstSelected <> nil) and (PTileInfo(vdtTiles.GetNodeData(vdtTiles.GetFirstSelected))^.ID >= $2F000000);

    if CurrentTile = nil then
      selectedRect := Rect(-1, -1, -1, -1)
    else
      selectedRect := GetSelectedRect;

    //clean up old ghost tiles
    //Logger.Send([lcClient, lcDebug], 'Cleaning ghost tiles');
    for i := FVirtualTiles.Count - 1 downto 0 do
    begin
      item := FVirtualTiles[i];
      if (item is TGhostTile) and not IsInRect((item as TGhostTile).CenterX,
         (item as TGhostTile).CenterY, selectedRect) then
      begin
        FScreenBuffer.Delete(item);
        FVirtualTiles.Delete(i);
      end;
    end;
    //Logger.Send([lcClient, lcDebug], 'FSelection', FSelection);
    if brushMod then begin // Для кистей
      i:= -1; inc(selectedRect.Left,  -1);  inc(selectedRect.Top,    -1);
    end else i:=  0;
    for tileX := FSelection.Left+i to FSelection.Right do
      for tileY := FSelection.Top+i to FSelection.Bottom do
        if not IsInRect(tileX, tileY, selectedRect) then
        begin
          cell := FLandscape.MapCell[tileX, tileY];
          if (cell <> nil) and cell.IsGhost then
          begin
            cell.IsGhost := False;
            PrepareMapCell(cell);
          end;
        end;
    if brushMod then begin // Для кистей
      inc(selectedRect.Left,  +1);  inc(selectedRect.Top,    +1);
    end;

    if (CurrentTile <> nil) and (not acSelect.Checked) then
    begin
      blockInfo := nil;
      if (SelectedTile <> nil) and (CurrentTile <> SelectedTile) then
      begin
        {Logger.Send([lcClient, lcDebug], 'Multiple Targets');
        Logger.Send([lcClient, lcDebug], 'SelectedRect', selectedRect);}
        //Logger.Send([lcClient, lcDebug], 'SelectedTile: %.5x (%.6d)', [SelectedTile.TileID, SelectedTile.TileID]);
        //set new ghost tiles

        if acDraw.Checked then begin
          for tileX := selectedRect.Left to selectedRect.Right do
            for tileY := selectedRect.Top to selectedRect.Bottom do
              if not IsInRect(tileX, tileY, FSelection) then
                AddGhostTile(tileX, tileY, SelectedTile, selectedRect);
          if brushMod then begin // Для кистей
            if (selectedRect.Left > FSelection.Left) then
              for tileY := selectedRect.Top to selectedRect.Bottom do
                AddGhostTile(selectedRect.Left, tileY, SelectedTile, selectedRect);
            if (selectedRect.Top > FSelection.Top) then
              for tileX := selectedRect.Left to selectedRect.Right do
                AddGhostTile(tileX, selectedRect.Top, SelectedTile, selectedRect);
            if (selectedRect.Right < FSelection.Right) then
              for tileY := selectedRect.Top to selectedRect.Bottom do
                AddGhostTile(selectedRect.Right, tileY, SelectedTile, selectedRect);
            if (selectedRect.Bottom < FSelection.Bottom) then
              for tileX := selectedRect.Left to selectedRect.Right do
                AddGhostTile(tileX, selectedRect.Bottom, SelectedTile, selectedRect);
            if (selectedRect.Right > FSelection.Right) then
              for tileY := FSelection.Top to FSelection.Bottom do
                AddGhostTile(FSelection.Right, tileY, SelectedTile, selectedRect);
            if (selectedRect.Bottom > FSelection.Bottom) then
              for tileX := FSelection.Left to FSelection.Right do
                AddGhostTile(tileX, FSelection.Bottom, SelectedTile, selectedRect);
          end;
        end;
        while FScreenBuffer.Iterate(blockInfo) do
          if (blockInfo^.State = ssNormal) then
            SetHighlight(blockInfo, IsInRect(blockInfo^.Item.X, blockInfo^.Item.Y,
              selectedRect) and not acDraw.Checked);
      end else
      begin
        //Logger.Send([lcClient, lcDebug], 'Single Target');
        //Logger.Send([lcClient, lcDebug], 'CurrentTile: %.5x (%.6d)', [CurrentTile.TileID, CurrentTile.TileID]);
        if acDraw.Checked and not IsInRect(CurrentTile.X, CurrentTile.Y, FSelection) then
          AddGhostTile(CurrentTile.X, CurrentTile.Y, CurrentTile, selectedRect);
        while FScreenBuffer.Iterate(blockInfo) do
          if blockInfo^.State = ssNormal then
            SetHighlight(blockInfo, (blockInfo^.Item = CurrentTile) and not acDraw.Checked);
      end;
    end;
    FSelection := selectedRect;
  end;
  {Logger.Send([lcClient, lcDebug], 'Virtual Tiles', FVirtualTiles.Count);
  Logger.ExitMethod([lcClient, lcDebug], 'UpdateSelection');}
end;

procedure TfrmMain.OnTileRemoved(ATile: TMulBlock);
begin
  if ATile = FCurrentTile then
    FCurrentTile := nil
  else if ATile = FSelectedTile then
    FSelectedTile := nil;
end;

procedure TfrmMain.WriteChatMessage(ASender, AMessage: string);
var
  node: PVirtualNode;
  chatInfo: PChatInfo;
begin
  node := vstChat.AddChild(nil);
  chatInfo := vstChat.GetNodeData(node);
  chatInfo^.Time := Now;
  chatInfo^.Sender := ASender;
  chatInfo^.Msg := AMessage;
  if vstChat.RootNodeCount > 30 then
    vstChat.DeleteNode(vstChat.GetFirst);
  vstChat.ScrollIntoView(node, False);
  
  if not pnlChat.Visible then
  begin
    lblChatHeaderCaption.Font.Bold := True;
    lblChatHeaderCaption.Font.Italic := True;
    lblChatHeaderCaption.Font.Color := clRed;
  end;
end;

procedure TfrmMain.OnClientHandlingPacket(ABuffer: TEnhancedMemoryStream);
var
  sender, msg: string;
  i: Integer;
  accessLevel: TAccessLevel;
  clientNode: PVirtualNode;
  clientInfo: PClientInfo;
begin
  case ABuffer.ReadByte of
    $01: //client connected
      begin
        sender := ABuffer.ReadStringNull;
        vstClients.BeginUpdate;
        clientNode := vstClients.AddChild(nil);
        clientInfo := vstClients.GetNodeData(clientNode);
        clientInfo^.Name := sender;
        clientInfo^.AccessLevel := TAccessLevel(ABuffer.ReadByte);
        clientInfo^.LogonDateTime := Now;
        vstClients.EndUpdate;
        if sender <> dmNetwork.Username then
          WriteChatMessage('System', Format(lbUserLoginedMsg, [sender]));
      end;
    $02:
      begin
        sender := ABuffer.ReadStringNull;
        vstClients.BeginUpdate;
        clientNode := vstClients.GetFirst;
        while clientNode <> nil do begin
          clientInfo := vstClients.GetNodeData(clientNode);
          if (clientInfo^.Name = sender)
          then begin
            vstClients.DeleteNode(clientNode);
            break;
          end
          else clientNode := vstClients.GetNext(clientNode);
        end;
        vstClients.EndUpdate;
        if sender <> dmNetwork.Username then
          WriteChatMessage('System', Format(lbUserLogoutedMsg, [sender]));
      end;
    $03: //Client list
      begin
        vstClients.Clear;
        while ABuffer.Position < ABuffer.Size do
        begin
          sender := ABuffer.ReadStringNull;
          clientNode := vstClients.AddChild(nil);
          clientInfo := vstClients.GetNodeData(clientNode);
          clientInfo^.Name := sender;
          clientInfo^.AccessLevel := TAccessLevel(ABuffer.ReadByte);
          clientInfo^.LogonDateTime := IncSecond(dmNetwork.ServerStart, ABuffer.ReadDWord);
        end;
      end;
    $04: //Set pos
      begin
        FX := ABuffer.ReadWord;
        FY := ABuffer.ReadWord;
        SetPos(FX, FY);
      end;
    $05: //chat
      begin
        sender := ABuffer.ReadStringNull;
        msg := ABuffer.ReadStringNull;
        WriteChatMessage(sender, msg);
      end;
    $07: //access changed
      begin
        accessLevel := TAccessLevel(ABuffer.ReadByte);
        FLandscape.UpdateWriteMap(ABuffer);
        FRepaintNeeded := True;

        if accessLevel <> dmNetwork.AccessLevel then
        begin
          dmNetwork.AccessLevel := accessLevel;
          if accessLevel = alNone then
          begin
            MessageDlg(lbDlgBlockedAccessCaption, lbDlgBlockedAccess, mtWarning, [mbOK], 0);
            mnuDisconnectClick(nil);
          end else
          begin
            ProcessAccessLevel;
            MessageDlg(lbDlgCnangedAccessCaption, Format(lbDlgCnangedAccess, [GetAccessLevel(accessLevel)]), mtWarning, [mbOK], 0);
          end;
        end;

        for i := FAccessChangedListeners.Count - 1 downto 0 do
          FAccessChangedListeners[i](accessLevel);
      end;
  end;
end;

function TfrmMain.GetInternalTileID(ATile: TWorldItem): LongWord;
begin
  Result := ATile.TileID;
  if ATile is TStaticItem then
    Inc(Result, $4000);
end;

function TfrmMain.GetSelectedRect: TRect;
begin
  if CurrentTile <> nil then
  begin
    if SelectedTile <> nil then
    begin
      Result.Left := Min(CurrentTile.X, SelectedTile.X);
      Result.Top := Min(CurrentTile.Y, SelectedTile.Y);
      Result.Right := Max(CurrentTile.X, SelectedTile.X);
      Result.Bottom := Max(CurrentTile.Y, SelectedTile.Y);
    end else
    begin
      Result.Left := CurrentTile.X;
      Result.Top := CurrentTile.Y;
      Result.Right := CurrentTile.X;
      Result.Bottom := CurrentTile.Y;
    end;
  end;
end;

function TfrmMain.ConfirmAction: Boolean;
begin
  if acMove.Checked and frmMoveSettings.cbAsk.Checked then
  begin
    Result := frmMoveSettings.ShowModal = mrYes;
  end else
  if not mnuSecurityQuestion.Checked then
  begin
    Result := True;
  end else
  begin
    frmConfirmation.Left := Mouse.CursorPos.x - frmConfirmation.btnYes.Left - frmConfirmation.btnYes.Width div 2;
    frmConfirmation.Top := Mouse.CursorPos.y - frmConfirmation.btnYes.Top - frmConfirmation.btnYes.Height div 2;
    Result := frmConfirmation.ShowModal = mrYes;
  end;

  if not oglGameWindow.MouseEntered then
    oglGameWindowMouseLeave(nil);
end;

function TfrmMain.FindRandomPreset(AName: String): TDOMElement;
var
  preset: TDOMElement;
  presets: TDOMNodeList;
  i: Integer;
begin
  presets := FRandomPresetsDoc.DocumentElement.ChildNodes;
  Result := nil;
  i := 0;
  while (i < presets.Count) and (Result = nil) do
  begin
    preset := TDOMElement(presets[i]);
    if SameText(preset.AttribStrings['Name'], AName) then
      Result := preset
    else
      Inc(i);
  end;
end;

procedure TfrmMain.ForceUpdateCurrentTile;
begin
  CurrentTile := nil;
  UpdateCurrentTile;
end;

procedure TfrmMain.GetDrawOffset(AX, AY: Integer; out DrawX, DrawY: Integer); inline;
var
  zoom: Single;
begin
  if tbZoom.Down then zoom := tbZoom.Tag / 1000.0 else zoom := 1.0;

  Dec(AX, FX);
  Dec(AY, FY);
  DrawX := (oglGameWindow.Width div 2) + Trunc((AX - AY) * 22 * zoom);
  DrawY := (oglGamewindow.Height div 2) + Trunc((AX + AY) * 22 * zoom);
end;

initialization
  {$I UfrmMain.lrs}

end.

