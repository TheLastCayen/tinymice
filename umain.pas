{
######################################################
#         Copyright 2020 Daniel Babin                #
######################################################

This file is part of TinyMice.

TinyMice is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TinyMice is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with TinyMice.  If not, see <https://www.gnu.org/licenses/>.es/>.
}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MouseAndKeyInput, LCLType, ExtCtrls, Menus, Grids, Buttons, DBGrids,
  DBCtrls, MaskEdit, ComCtrls, Spin, uprofilename, udeleteprofile, uAbout,
  uoptions, DefaultTranslator,lcltranslator, translations, FileUtil, LazFileUtils,  LazUTF8,
  {$IFDEF Windows}
    windows,
  {$ENDIF}
  {$IFDEF UNIX}
    Codebot.Input.Hotkeys,
  {$ENDIF}
  sqldb, sqlite3conn, db;

type

  { TFMain }

  TFMain = class(TForm)
    BAdd: TBitBtn;
    BTest: TBitBtn;
    BProfileDelete: TBitBtn;
    BNew: TBitBtn;
    BDelete: TBitBtn;
    ETest: TEdit;
    ImageList1: TImageList;
    LSingleClick: TLabel;
    LProfileClick: TLabel;
    LPanel: TLabel;
    DBGProfiles: TDBGrid;
    DBGMouse: TDBGrid;
    MainMenu1: TMainMenu;
    SMILanguages: TMenuItem;
    N3: TMenuItem;
    MILanguages: TMenuItem;
    SMIProfiles: TMenuItem;
    SMIAbout: TMenuItem;
    SMIQuit: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    SMIOptions: TMenuItem;
    SMIShow: TMenuItem;
    MIOptions: TMenuItem;
    MIFile: TMenuItem;
    MIQuit: TMenuItem;
    MITools: TMenuItem;
    MIHelp: TMenuItem;
    MIAbout: TMenuItem;
    Panel1: TPanel;
    PMSystray: TPopupMenu;
    SESingleClick: TSpinEdit;
    SEProfileClick: TSpinEdit;
    TimerProfile: TTimer;
    TimerSimpleClick: TTimer;
    TrayIcon1: TTrayIcon;
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BTestClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure BProfileDeleteClick(Sender: TObject);
    procedure DBGMouseEditingDone(Sender: TObject);
    procedure DBGMouseEnter(Sender: TObject);
    procedure DBGProfilesCellClick(Column: TColumn);
    procedure DBGProfilesEditingDone(Sender: TObject);
    procedure DBGProfilesEnter(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIOptionsClick(Sender: TObject);
    procedure MIQuitClick(Sender: TObject);
    procedure SEProfileClickEditingDone(Sender: TObject);
    procedure TimerProfileStartTimer(Sender: TObject);
    procedure TimerProfileStopTimer(Sender: TObject);
    procedure TimerProfileTimer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    function  ValidateMouseAction(MAction: string):Boolean;
    procedure RegisterKeyHook;
    procedure UnregisterKeyHook;
    {$IFDEF UNIX}
    procedure KeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF Windows}
    Class Function AHookProc(nCode:LongInt; wParam:WPARAM; Lparam:LParam):LRESULT; StdCall; Static;
    {$ENDIF}
    function ClickType(vclick:string): TMouseButton;
    procedure TimerSimpleClickStartTimer(Sender: TObject);
    procedure TimerSimpleClickStopTimer(Sender: TObject);
    procedure TimerSimpleClickTimer(Sender: TObject);
  private

    procedure SimpleAutoClick;
    procedure PlayProfile;
    procedure AddToProfile;
    procedure DelFromProfile;
    procedure CreateDB;
    Procedure PopulateDB;
    Procedure LoadOptions;
    Procedure RefreshProfiles;
    Procedure RefreshMouse;
    procedure EnableFunctions;
    procedure DisableFunctions;
    procedure AddProfileToSystray(ProfileName: String; Selected:Boolean);
    procedure EmptySystrayProfiles;
    procedure SelectProfile(Sender: TObject);
    function FindLanguageFiles:TStringList;
    procedure CreateLanguageMenuItems;
    function SelectLanguageFlag(Language:String): Integer;
    procedure ChangeLanguage(Sender: TObject);
  public
    Const
      database = 'data.db';

  end;

var
  FMain: TFMain;

implementation
{$R *.lfm}

{ TFMain }
Const
  WH_KEYBOARD_LL = 13;

type
  TFkey = record
    SingleClick  : Integer;
    ProfileClick : Integer;
    SaveMouse    : Integer;
    DeleteMouse  : Integer;
  end;

  TFKeyEnabled = record
    SimpleClick   : Boolean;
    ProfileClick  : Boolean;
    ProfileRecord : Boolean;
  end;

  TOptions = record
    StartMinimized : Boolean;
    CloseMinimized : Boolean;
    SysTray   : Boolean;
    Fkey      : TFkey;
  end;


  KeybdLLHookStruct = record
    vkCode      : cardinal;
    scanCode    : cardinal;
    flags       : cardinal;
    time        : cardinal;
    dwExtraInfo : cardinal;
  end;

var
  DBConnection   : TSQLite3Connection;
  SQLTransaction : TSQLTransaction;
  SQLQuery       : TSQLQuery;
  SQLProfiles    : TSQLQuery ;
  SQLMouse       : TSQLQuery ;
  DataProfiles   : TDataSource;
  DataMouse      : TDataSource;
  HookHandle     : Int64;
  FKeyEnabled    : TFKeyEnabled;
  VOptions       : TOptions;

resourcestring
  CDeleteprofile = 'Delete profile : ';
  CProfile       = 'Profile';
  CMouseAction   = 'Mouse Action';
  CMouseX        = 'Mouse X';
  CMouseY        = 'Mouse Y';


procedure TFMain.FormCreate(Sender: TObject);
begin
  //Load Language Files
  MILanguages.Clear;
  SMILanguages.Clear;
  CreateLanguageMenuItems;

  //Set Fkey Disabled by default
  FKeyEnabled.SimpleClick  := False;
  FKeyEnabled.ProfileClick := False;
  FKeyEnabled.ProfileRecord:= False;

  DBGProfiles.AlternateColor:=clSkyBlue;
  DBGMouse.AlternateColor := clSkyBlue;
  //Database Connections
  DBConnection   := TSQLite3Connection.Create(nil);
  SQLTransaction := TSQLTransaction.Create(nil);
  SQLQuery       := TSQLQuery.Create(nil);
  SQLProfiles    := TSQLQuery.Create(nil);
  SQLMouse       := TSQLQuery.Create(nil);
  DataProfiles   := TDataSource.Create(nil);
  DataMouse      := TDataSource.Create(nil);
  if not DirectoryExists(GetAppConfigDir(False)) then
    CreateDir(GetAppConfigDir(False));

  With  DBConnection do
    begin
      HostName:='';
      DatabaseName:=GetAppConfigDir(False)+database;
      UserName:='SYSDBA';
      Password:='apple';
      Charset := 'UTF8';
      Params.Add('PAGE_SIZE=16384');
      Transaction := SQLTransaction;
    end;

  SQLQuery.DataBase      := DBConnection;
  SQLProfiles.DataBase   := DBConnection;
  SQLMouse.DataBase      := DBConnection;
  DataProfiles.DataSet   := SQLProfiles;
  DataMouse.DataSet      := SQLMouse;
  DBGProfiles.DataSource := DataProfiles;
  DBGMouse.DataSource    := DataMouse;

  if not(FileExists(DBConnection.DatabaseName)) then
    CreateDB;
  RefreshProfiles;

  EnableFunctions;

  LoadOptions;
  RegisterKeyHook;

end;
procedure TFMain.FormDestroy(Sender: TObject);
begin
  UnregisterKeyHook;

  DataMouse.free;
  DataProfiles.free;
  SQLMouse.free;
  SQLProfiles.free;
  SQLQuery.free;
  SQLTransaction.free;
  DBConnection.free;
end;

procedure TFMain.FormWindowStateChange(Sender: TObject);
begin
  if VOptions.SysTray then
    Application.MainForm.visible := not Application.Mainform.Visible;
end;

procedure TFMain.MIAboutClick(Sender: TObject);
Var
  VAbout:TFAbout;
begin
   VAbout:=TFAbout.Create(nil);
  try
    VAbout.ShowModal;
  finally
    VAbout.Free;
  end;
End;

procedure TFMain.MIOptionsClick(Sender: TObject);
Var
  VOption:TFOptions;
begin
  UnregisterKeyHook;
  DisableFunctions;

  VOption:=TFOptions.Create(nil);
  try
    VOption.ShowModal;
  finally
    VOption.Free;
  end;

  DBConnection.Connected := True;
  SQLTransaction.Active := True;
  RefreshProfiles;
  RefreshMouse;
  RegisterKeyHook;
  LoadOptions;
  EnableFunctions;
  FMain.WindowState := wsNormal;
  FMain.Show;
end;

procedure TFMain.BNewClick(Sender: TObject);
Var
  VProfileName:TFProfileName;

begin
  VProfileName:=TFProfileName.Create(nil);
  try
    if VProfileName.ShowModal = mrOK then
      begin
        SQLQuery.SQL.Clear;
        SQLQuery.SQL.Add('insert into "PROFILES" ');
        SQLQuery.SQL.Add('("Profiles","ClickTimer") values ');
        SQLQuery.SQL.Add('('''+VProfileName.EProfileName.Text+''','''+VProfileName.SPClickInterval.Text+''')');
        SQLQuery.ExecSQL;
        SQLTransaction.Commit;
        RefreshProfiles;
        DBGProfiles.DataSource.DataSet.Last;
        SMIProfiles.Items[DBGProfiles.DataSource.DataSet.RecNo-1].Checked:=true;
        RefreshMouse;
      end;
  finally
    VProfileName.Free;
  end;
  EnableFunctions;
end;

procedure TFMain.BAddClick(Sender: TObject);
begin
 With SQLQuery do
   begin
     SQL.Clear;
     SQL.Add('INSERT INTO "MOUSE" ');
     SQL.Add('("MouseAction","MouseX","MouseY","Profiles_ID")');
     SQL.ADD('VALUES (');
     SQL.ADD(''''+DBGMouse.Columns.Items[2].PickList[0]+''', ');
     SQL.ADD(''''', ');
     SQL.ADD(''''', ');
     SQL.ADD(''''+inttostr(DBGProfiles.DataSource.DataSet.Fields[0].Value)+''');');
     ExecSQL;
   end;
 SQLTransaction.CommitRetaining;
 DBGMouse.DataSource.DataSet.Refresh;
 DBGMouse.DataSource.DataSet.Last;
 EnableFunctions;
end;

procedure TFMain.BDeleteClick(Sender: TObject);
begin
  if not(DBGMouse.DataSource.DataSet.Fields[0].Value = NULL) then
    begin
      With SQLQuery do
        begin
          SQL.Clear;
          SQL.ADD('DELETE FROM "MOUSE" ');
          SQL.ADD('WHERE ');
          SQL.ADD('"ID" = '+DBGMouse.DataSource.DataSet.Fields[0].AsString+';');
          ExecSQL;
        end;
      SQLTransaction.CommitRetaining;
      DBGMouse.DataSource.DataSet.Refresh;
    end;
  EnableFunctions;
end;

procedure TFMain.BTestClick(Sender: TObject);
begin
  ETest.text := inttostr(StrToInt(ETest.text)+1);
  AddProfileToSystray('Random Name '+inttostr(Random(5000)), True);
end;

procedure TFMain.BProfileDeleteClick(Sender: TObject);
Var
  VDeleteProfile:TFDeleteProfile;
begin
  if not(DBGProfiles.DataSource.DataSet.Fields[0].Value = NULL) then
    begin
      VDeleteProfile := TFDeleteProfile.Create(nil);
      try
        begin
          VDeleteProfile.Caption:=CDeleteprofile+DBGProfiles.DataSource.DataSet.Fields[2].Value;
          VDeleteProfile.LConfirm4.Caption := DBGProfiles.DataSource.DataSet.Fields[2].Value;
          if VDeleteProfile.ShowModal = mrOK then
            begin
              SQLQuery.SQL.Clear;
              SQLQuery.SQL.ADD('DELETE FROM "MOUSE" ');
              SQLQuery.SQL.ADD('WHERE ');
              SQLQuery.SQL.ADD('"Profiles_ID" = '+inttostr(DBGProfiles.DataSource.DataSet.Fields[0].Value)+';');
              SQLQuery.ExecSQL;

              SQLQuery.SQL.Clear;
              SQLQuery.SQL.ADD('DELETE FROM "PROFILES" ');
              SQLQuery.SQL.ADD('WHERE ');
              SQLQuery.SQL.ADD('"ID" = '+inttostr(DBGProfiles.DataSource.DataSet.Fields[0].Value)+';');
              SQLQuery.ExecSQL;

              SQLTransaction.Commit;
            end;
        end;
      finally
        VDeleteProfile.Free;
      end;
     RefreshProfiles;
    end;
  EnableFunctions;
end;

procedure TFMain.DBGMouseEditingDone(Sender: TObject);
begin
  if not(DBGMouse.DataSource.DataSet.Fields[0].Value = NULL) then
    begin
      SQLMouse.Edit;
      SQLMouse.Post;
      SQLMouse.ApplyUpdates;
      SQLTransaction.CommitRetaining;
    end;
end;

procedure TFMain.DBGMouseEnter(Sender: TObject);
begin
  EnableFunctions;
end;

procedure TFMain.DBGProfilesCellClick(Column: TColumn);
begin
  if not(DBGProfiles.DataSource.DataSet.Fields[0].Value = NULL )then
    begin
      SEProfileClick.Text:=DBGProfiles.DataSource.DataSet.Fields[3].Value;
      if SMIProfiles.Count > DBGProfiles.DataSource.DataSet.RecNo -1 then
        SMIProfiles.Items[DBGProfiles.DataSource.DataSet.RecNo-1].Checked:=true;
      RefreshMouse;
    end;
  EnableFunctions;
end;

procedure TFMain.DBGProfilesEditingDone(Sender: TObject);
begin
 if not(DBGProfiles.DataSource.DataSet.Fields[0].Value = NULL) then
   begin
     SQLProfiles.Edit;
     SQLProfiles.Post;
     SQLProfiles.ApplyUpdates;
     SQLTransaction.CommitRetaining;
   end;
end;

procedure TFMain.DBGProfilesEnter(Sender: TObject);
begin
  EnableFunctions;
end;

procedure TFMain.FormActivate(Sender: TObject);
begin
 if VOptions.StartMinimized then
   Application.Minimize;
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

 if VOptions.CloseMinimized then
   begin
     if VOptions.SysTray then
       CloseAction := caHide
     else
       CloseAction:= caMinimize;
     Application.Minimize;
   end;
end;

procedure TFMain.MIQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFMain.SEProfileClickEditingDone(Sender: TObject);
begin
  SQLProfiles.Edit;
  DBGProfiles.DataSource.DataSet.Fields[3].Value:=SEProfileClick.Value;
  SQLProfiles.Post;
  SQLProfiles.ApplyUpdates;
  SQLTransaction.CommitRetaining;
end;

procedure TFMain.TimerProfileStartTimer(Sender: TObject);
begin
  FKeyEnabled.SimpleClick   := False;
  FKeyEnabled.ProfileRecord := False;
  SESingleClick.Enabled     := False;
  Panel1.Enabled            := False;
  MIOptions.Enabled         := False;
  DBGMouse.DataSource.DataSet.First;
  DBGMouse.DataSource.DataSet.DisableControls;
end;

procedure TFMain.TimerProfileStopTimer(Sender: TObject);
begin
  SESingleClick.Enabled     := True;
  Panel1.Enabled            := True;
  MIOptions.Enabled         := True;
  EnableFunctions;
  DBGMouse.DataSource.DataSet.EnableControls;
end;

procedure TFMain.TimerProfileTimer(Sender: TObject);
begin
  if
    ValidateMouseAction(DBGMouse.DataSource.DataSet.Fields[2].Value) and
    (StrToIntDef(DBGMouse.DataSource.DataSet.Fields[3].Value,-1) >  -1) and
    (StrToIntDef(DBGMouse.DataSource.DataSet.Fields[3].Value,Screen.DesktopWidth+1) <  Screen.DesktopWidth+1) and
    (StrToIntDef(DBGMouse.DataSource.DataSet.Fields[4].Value,-1) >  -1) and
    (StrToIntDef(DBGMouse.DataSource.DataSet.Fields[4].Value,Screen.DesktopHeight+1) <  Screen.DesktopHeight+1)
  then
    MouseInput.Click(
      ClickType(DBGMouse.DataSource.DataSet.Fields[2].Value),
      [],
      strtoint(DBGMouse.DataSource.DataSet.Fields[3].Value),
      strtoint(DBGMouse.DataSource.DataSet.Fields[4].Value)
    );

  if DBGMouse.DataSource.DataSet.EOF then
    DBGMouse.DataSource.DataSet.First
  else
    DBGMouse.DataSource.DataSet.Next;
end;

procedure TFMain.TrayIcon1DblClick(Sender: TObject);
begin
  if FMain.Visible then
    FMain.Hide
  else if not FMain.Visible then
    begin
      FMain.WindowState := wsNormal;
      FMain.Show;
    end;
end;

function  TFMain.ValidateMouseAction(MAction: string):Boolean;
Var
  I:Byte;
begin
  Result := False;
  For I := 0 to DBGMouse.Columns.Items[2].PickList.Count - 1 do
    If MAction = DBGMouse.Columns.Items[2].PickList.Strings[I] then Result := True;
end;
procedure TFMain.RegisterKeyHook;
begin
  LoadOptions;

 {$IFDEF Windows}
   UnhookWindowsHookEx(HookHandle);
   HookHandle := SetWindowsHookEx(WH_KEYBOARD_LL, HookPROC(@TFMain.AHookProc),HInstance, 0);
 {$ENDIF}

 {$IFDEF UNIX}
   HotkeyCapture.RegisterNotify(VOptions.Fkey.SingleClick,[],@KeyPressed);
   HotkeyCapture.RegisterNotify(VOptions.Fkey.ProfileClick,[],@KeyPressed);
   HotkeyCapture.RegisterNotify(VOptions.Fkey.SaveMouse,[],@KeyPressed);
   HotkeyCapture.RegisterNotify(VOptions.Fkey.DeleteMouse,[],@KeyPressed);
 {$ENDIF}
end;

procedure TFMain.UnregisterKeyHook;
Begin
  {$IFDEF Windows}
    UnhookWindowsHookEx(HookHandle);
  {$ENDIF}

  {$IFDEF UNIX}
    HotkeyCapture.UnRegisterNotify(VOptions.Fkey.SingleClick,[]);
    HotkeyCapture.UnRegisterNotify(VOptions.Fkey.ProfileClick,[]);
    HotkeyCapture.UnRegisterNotify(VOptions.Fkey.SaveMouse,[]);
    HotkeyCapture.UnRegisterNotify(VOptions.Fkey.DeleteMouse,[]);
  {$ENDIF}
end;

////////////////////////////Linux FKey//////////////////////////////////////////
{$IFDEF UNIX}
procedure TFMain.KeyPressed(Sender: TObject; Key: Word; Shift: TShiftState);
begin

  With VOptions do
    begin
      if Key = Fkey.SingleClick  then FMain.SimpleAutoClick;
      if Key = Fkey.ProfileClick then FMain.PlayProfile;
      if Key = Fkey.SaveMouse    then FMain.AddToProfile;
      if Key = Fkey.DeleteMouse  then FMain.DelFromProfile;
    end;

end;
{$ENDIF}
{$IFDEF Windows}
////////////////////////////Windows FKey////////////////////////////////////////
class Function TFMain.AHookProc(nCode:LongInt; wParam:WPARAM;Lparam:LParam):LRESULT; StdCall;
var
  keyboard : ^KeybdLLHookStruct absolute lParam;
begin
  Result :=  CallnextHookEx(HookHandle,nCode,wParam, lparam);
  with VOptions do
    if wParam = WM_KEYDOWN then
      begin
        if keyboard^.vkCode = Fkey.SingleClick  then FMain.SimpleAutoClick;
        if keyboard^.vkCode = Fkey.ProfileClick then FMain.PlayProfile;
        if keyboard^.vkCode = Fkey.SaveMouse    then FMain.AddToProfile;
        if keyboard^.vkCode = Fkey.DeleteMouse  then FMain.DelFromProfile;
      end;
end;
{$ENDIF}

procedure TFMain.SimpleAutoClick;
begin
  If FKeyEnabled.SimpleClick then
    begin
      DBGMouse.Options         := DBGMouse.Options-[dgEditing];
      DBGProfiles.Options      := DBGProfiles.Options-[dgEditing];
      EnableFunctions;
      If TimerSimpleClick.Enabled = False then
        begin
          TimerSimpleClick.Interval:=SESingleClick.Value;
          TimerSimpleClick.Enabled := True;
        end
      else
        TimerSimpleClick.Enabled := False;

    end;
end;

procedure TFMain.PlayProfile;
begin
  If FKeyEnabled.ProfileClick then
    begin
      DBGMouse.Options         := DBGMouse.Options-[dgEditing];
      DBGProfiles.Options      := DBGProfiles.Options-[dgEditing];
      EnableFunctions;
      If TimerProfile.Enabled = False then
        begin
          TimerProfile.Interval:= SEProfileClick.Value;
          TimerProfile.Enabled := True;
        end
      else
        TimerProfile.Enabled := False;
    end;
end;

procedure TFMain.AddToProfile;
begin
  if FKeyEnabled.ProfileRecord then
    begin
      With SQLQuery do
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO "MOUSE" ');
          SQL.Add('("MouseAction","MouseX","MouseY","Profiles_ID")');
          SQL.ADD('VALUES (');
          SQL.ADD(''''+DBGMouse.Columns.Items[2].PickList[0]+''', ');
          SQL.ADD(''''+inttostr(Mouse.CursorPos.x)+''', ');
          SQL.ADD(''''+inttostr(Mouse.CursorPos.Y)+''', ');
          SQL.ADD(''''+inttostr(DBGProfiles.DataSource.DataSet.Fields[0].Value)+''');');
          ExecSQL;
        end;
      SQLTransaction.CommitRetaining;
      DBGMouse.DataSource.DataSet.Refresh;
      DBGMouse.DataSource.DataSet.Last;
      EnableFunctions;
    end;
end;

procedure TFMain.DelFromProfile;
var
  ID: String;
begin
  if FKeyEnabled.ProfileRecord then
    begin
      if not(DBGMouse.DataSource.DataSet.Fields[0].Value = NULL) then
        begin
          With SQLQuery do
            begin
              SQL.Clear;
              SQL.Add('SELECT ID ');
              SQL.Add('FROM MOUSE ');
              SQL.ADD('WHERE ');
              SQL.ADD('"Profiles_ID" = '+inttostr(DBGProfiles.DataSource.DataSet.Fields[0].Value)+' ');
              SQL.Add('ORDER BY "ID" DESC;');
              Open;

              ID:= SQLQuery.FieldByName('ID').AsString;
              Close;

              SQL.Clear;
              SQL.ADD('DELETE FROM "MOUSE" ');
              SQL.ADD('WHERE ');
              SQL.ADD('"ID" = '+ID+';');
              ExecSQL;
            end;
          SQLTransaction.CommitRetaining;
          DBGMouse.DataSource.DataSet.Refresh;
          DBGMouse.DataSource.DataSet.Last;
        end;
      EnableFunctions;
    end;
end;

function TFMain.ClickType(vclick:string): TMouseButton;
begin
  case vclick of
    'mbLeft': Result:= mbLeft;
    'mbRight': Result:= mbRight;
    'mbMiddle': Result:= mbMiddle;
    'mbExtra1': Result:= mbExtra1;
    'mbExtra2': Result:= mbExtra2;
    else Result:= mbLeft;
  end;
end;

procedure TFMain.TimerSimpleClickStartTimer(Sender: TObject);
begin
  FKeyEnabled.ProfileClick  := False;
  FKeyEnabled.ProfileRecord := False;
  SESingleClick.Enabled     := False;
  Panel1.Enabled            := False;
  MIOptions.Enabled         := False;
end;

procedure TFMain.TimerSimpleClickStopTimer(Sender: TObject);
begin
  SESingleClick.Enabled     := True;
  Panel1.Enabled            := True;
  MIOptions.Enabled         := True;
  EnableFunctions;
end;

procedure TFMain.TimerSimpleClickTimer(Sender: TObject);
begin
  MouseInput.Click(mbLeft,[],Mouse.CursorPos.x,Mouse.CursorPos.y);
end;

procedure TFMain.CreateDB;
begin
  DBConnection.CreateDB;
  With SQLQuery do
    begin
      SQL.Clear;
      SQL.ADD('CREATE TABLE "OPTIONS"(');
      SQL.ADD('"ID" INTEGER NOT NULL PRIMARY KEY,');
      SQL.ADD('"StartMinimized"  SMALLINT,');
      SQL.ADD('"CloseMinimized"  SMALLINT,');
      SQL.ADD('"SysTray"  SMALLINT,');
      SQL.ADD('"SingleClick"  SMALLINT,');
      SQL.ADD('"ProfileClick"  SMALLINT,');
      SQL.ADD('"SaveMouse"  SMALLINT,');
      SQL.ADD('"DeleteMouse"  SMALLINT);');
      ExecSQL;
      SQL.Clear;

      SQL.Clear;
      SQL.ADD('CREATE TABLE "PROFILES"(');
      SQL.ADD('"ID" INTEGER NOT NULL PRIMARY KEY,');
      SQL.ADD('"GridPosition"  int,');
      SQL.ADD('"Profiles"  varchar(100),');
      SQL.ADD('"ClickTimer"  varchar(7));');
      ExecSQL;

      SQL.Clear;
      SQL.ADD('CREATE TABLE "MOUSE" (');
      SQL.ADD('"ID" INTEGER NOT NULL PRIMARY KEY,');
      SQL.ADD('"GridPosition"  int,');
      SQL.ADD('"MouseAction"  varchar(20),');
      SQL.ADD('"MouseX" varchar(7),');
      SQL.ADD('"MouseY" varchar(7),');
      SQL.ADD('"ClickTimer"  SMALLINT,');
      SQL.ADD('"Profiles_ID" int,');
      SQL.ADD(' FOREIGN KEY ("Profiles_ID") REFERENCES PROFILES("ID"));');
      ExecSQL;

  end;
  SQLTransaction.Commit;
  SQLQuery.Close;
  DBConnection.Close;
  PopulateDB;
end;
Procedure TFMain.PopulateDB;
begin
  With SQLQuery do
    begin
      SQL.Clear;
      SQL.Add('INSERT INTO "OPTIONS"(');
      SQL.Add('"StartMinimized", ');
      SQL.Add('"CloseMinimized", ');
      SQL.Add('"SysTray", ');
      SQL.Add('"SingleClick", ');
      SQL.Add('"ProfileClick", ');
      SQL.Add('"SaveMouse", ');
      SQL.Add('"DeleteMouse"');
      SQL.Add(') values (');
      SQL.Add('''0'', ');
      SQL.Add('''0'', ');
      SQL.Add('''0'', ');
      SQL.Add(''''+inttostr(VK_F4)+''', ');
      SQL.Add(''''+inttostr(VK_F5)+''', ');
      SQL.Add(''''+inttostr(VK_F7)+''', ');
      SQL.Add(''''+inttostr(VK_F8)+'''');
      SQL.Add(');');
      ExecSQL;
    end;
  SQLTransaction.Commit;
end;
Procedure TFMain.LoadOptions;
var
  SQLOption: TSQLQuery;
begin
 // Load Settings
  SQLOption := TSQLQuery.Create(nil);
  With SQLQuery do
    begin
      DataBase := DBConnection;
      Close;
      SQL.Clear;
      SQL.Add('SELECT ');
      SQL.Add('"StartMinimized", ');
      SQL.Add('"CloseMinimized", ');
      SQL.Add('"SysTray", ');
      SQL.Add('"SingleClick", ');
      SQL.Add('"ProfileClick", ');
      SQL.Add('"SaveMouse", ');
      SQL.Add('"DeleteMouse" ');
      SQL.Add('FROM OPTIONS; ');
      Open;
      with VOptions do
        begin

          StartMinimized    := FieldByName('StartMinimized').AsBoolean;
          CloseMinimized    := FieldByName('CloseMinimized').AsBoolean;
          SysTray           := FieldByName('SysTray').AsBoolean;

          Fkey.SingleClick  := FieldByName('SingleClick').AsInteger;
          Fkey.ProfileClick := FieldByName('ProfileClick').AsInteger;
          Fkey.SaveMouse    := FieldByName('SaveMouse').AsInteger;
          Fkey.DeleteMouse  := FieldByName('DeleteMouse').AsInteger;

          If SysTray then
            begin
              TrayIcon1.Visible   := True;
              FMain.ShowInTaskBar := stNever;
            end
          else
            begin
              TrayIcon1.Visible:=False;
              FMain.ShowInTaskBar := stDefault;
            end;

        end;
      Close;
    end;
  SQLOption.Free;
end;

Procedure TFMain.RefreshProfiles;
begin
  With SQLProfiles do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT "ID", "GridPosition", "Profiles", "ClickTimer" ');
      SQL.Add('FROM PROFILES ');
      SQL.Add('ORDER BY "ID" ASC');
      Open;
    end;
  DBGProfiles.Columns[0].Visible := False;
  DBGProfiles.Columns[1].Visible := False;
  DBGProfiles.Columns[3].Visible := False;
  DBGProfiles.Columns.Items[2].Title.Alignment := taCenter;
  DBGProfiles.Columns.Items[2].Title.Caption   := CProfile;
  DBGProfilesCellClick(DBGProfiles.Columns[2]);

  EmptySystrayProfiles;
  If DBGProfiles.DataSource.DataSet.RecordCount>0 then
    begin
      DBGProfiles.DataSource.DataSet.DisableControls;
      While not DBGProfiles.DataSource.DataSet.EOF do
        begin
          AddProfileToSystray(DBGProfiles.DataSource.DataSet.Fields[2].Value, False);
          DBGProfiles.DataSource.DataSet.Next;
        end;
      DBGProfiles.DataSource.DataSet.First;
      DBGProfiles.DataSource.DataSet.EnableControls;
      SMIProfiles.Items[DBGProfiles.DataSource.DataSet.RecNo-1].Checked:=true;
    end;

end;
Procedure TFMain.RefreshMouse;
begin
 If DBGProfiles.DataSource.DataSet.RecordCount>0 then
   begin
     With SQLMouse do
       begin
         Close;
         SQL.Clear;
         SQL.Add('SELECT ');
         SQL.Add('"ID", ');
         SQL.Add('"GridPosition", ');
         SQL.Add('"MouseAction", ');
         SQL.Add('"MouseX", ');
         SQL.Add('"MouseY", ');
         SQL.Add('"ClickTimer" ');
         SQL.Add('FROM MOUSE ');
         SQL.Add('WHERE  "Profiles_ID" = '+inttostr(DBGProfiles.DataSource.DataSet.Fields[0].Value)+' ');
         SQL.Add('ORDER BY "ID" ASC');
         Open;
       end;
     with DBGMouse do
       begin

         Columns[0].Visible         := False;
         Columns[1].Visible         := False;
         Columns[5].Visible         := False;
         Columns[2].Title.Alignment := taCenter;
         Columns[3].Title.Alignment := taCenter;
         Columns[4].Title.Alignment := taCenter;
         Columns[2].ButtonStyle     := cbsPickList;
         Columns[2].Title.Caption   := CMouseAction;
         Columns[3].Title.Caption   := CMouseX;
         Columns[4].Title.Caption   := CMouseY;
         Columns[2].SizePriority    := 1;
         Columns[3].SizePriority    := 0;
         Columns[4].SizePriority    := 0;
         Columns[2].PickList.Clear;
         Columns[2].PickList.Add('mbLeft');
         Columns[2].PickList.Add('mbRight');
         Columns[2].PickList.Add('mbMiddle');
         Columns[2].PickList.Add('mbExtra1');
         Columns[2].PickList.Add('mbExtra2');
         AutoAdjustColumns;
       end;
   end;
end;

Procedure TFMain.EnableFunctions;
begin

  //Load Settings
  LoadOptions;


  if DBGProfiles.DataSource.DataSet.Fields[0].Value = NULL then
    begin
      SEProfileClick.Enabled   := False;
      BProfileDelete.Enabled   := False;
      DBGMouse.Enabled         := False;
      BAdd.Enabled             := False;
      BDelete.Enabled          := False;
      DBGMouse.Options         := DBGMouse.Options-[dgEditing];
      DBGProfiles.Options      := DBGProfiles.Options-[dgEditing];
      DBGMouse.Options := DBGMouse.Options+[dgEditing];
      FKeyEnabled.SimpleClick  := True;
      FKeyEnabled.ProfileClick := False;
      FKeyEnabled.ProfileRecord:= False;
    end
  else
    begin
      SEProfileClick.Enabled  := True;
      BProfileDelete.Enabled  := True;
      DBGMouse.Enabled        := True;
      DBGProfiles.Options := DBGProfiles.Options+[dgEditing];
      FKeyEnabled.SimpleClick  := True;
      if DBGMouse.DataSource.DataSet.FieldCount > -1 then
        begin
          FKeyEnabled.ProfileRecord:= True;
          if DBGMouse.DataSource.DataSet.Fields[0].Value = NULL then
            begin
              BAdd.Enabled            := True;
              BDelete.Enabled         := False;
              DBGMouse.Options := DBGMouse.Options-[dgEditing];
              FKeyEnabled.ProfileClick := False;
            end
          else
            begin
              BAdd.Enabled             := True;
              BDelete.Enabled          := True;
              DBGMouse.Options := DBGMouse.Options+[dgEditing];
              FKeyEnabled.ProfileClick := True;
            end;
        end;
    end;
end;
procedure TFMain.DisableFunctions;
begin
  TimerSimpleClick.Enabled := False;
  TimerProfile.Enabled     := False;
  FKeyEnabled.SimpleClick  := False;
  FKeyEnabled.ProfileClick := False;
  FKeyEnabled.ProfileRecord:= False;

  SQLQuery.Close;
  SQLMouse.Close;
  SQLProfiles.Close;
  SQLTransaction.Active := False;
  DBConnection.Connected := False;
end;

procedure TFMain.AddProfileToSystray(ProfileName: String; Selected:Boolean);
var
  TIProfile: TMenuItem;
begin
  TIProfile := TMenuItem.Create(Self);
  TIProfile.Caption   := ProfileName;
  SMIProfiles.Add(TIProfile);
  TIProfile.RadioItem := True;
  TIProfile.Checked   := Selected;
  TIProfile.OnClick   := @SelectProfile;
end;

procedure TFMain.EmptySystrayProfiles;
begin
  SMIProfiles.Clear;
end;

procedure TFMain.SelectProfile(Sender: TObject);
var
  I : integer;
begin
  if Sender.ClassType = TMenuItem then
    begin
      TMenuItem(Sender).Checked:=True;
      DBGProfiles.DataSource.DataSet.DisableControls;
      DBGProfiles.DataSource.DataSet.First;

      For I := 0 to TMenuItem(Sender).MenuIndex-1 do
        DBGProfiles.DataSource.DataSet.Next;
      DBGProfiles.DataSource.DataSet.EnableControls;
    end;
  RefreshMouse;
end;

function TFMain.FindLanguageFiles:TStringList;
begin
  Result := TStringList.Create;
  Result := FindAllFiles(ExtractFilePath(Application.ExeName)+'languages','*.mo',False);
end;
procedure TFMain.CreateLanguageMenuItems;
var
  TMILanguage : TMenuItem;
  TSILanguage : TMenuItem;
  LFiles     : TStringList;
  tmpstring  : string;
  I          : Integer;
  Lang, FallbackLang: String;
begin
  LazGetLanguageIDs(Lang,FallbackLang);
  LFiles := FindLanguageFiles;
  try
    For I := 0 to LFiles.Count -1 do
      begin
        LFiles[I] := ExtractFileNameOnly(LFiles[I]);
        tmpstring := UpperCase(StringReplace(LFiles[I], 'tinymice.','',[rfReplaceAll, rfIgnoreCase]));
        if trim(tmpstring) <> '' then
          begin
            TMILanguage := TMenuItem.Create(Self);
            TSILanguage := TMenuItem.Create(Self);
            TMILanguage.Caption := tmpstring;
            TSILanguage.Caption := tmpstring;
            MILanguages.Add(TMILanguage);
            SMILanguages.Add(TSILanguage);
            TMILanguage.ImageIndex:=SelectLanguageFlag(tmpstring);
            TSILanguage.ImageIndex:=SelectLanguageFlag(tmpstring);
            TMILanguage.RadioItem := True;
            TSILanguage.RadioItem := True;
            If pos(tmpstring,Lang) > 0 then
              begin
                 TMILanguage.Checked := True;
                 TSILanguage.Checked := True;
              end;
            TMILanguage.OnClick := @ChangeLanguage;
            TSILanguage.OnClick := @ChangeLanguage;
          end;
      end;

  finally
    if Assigned(LFiles) then
      FreeAndNil(LFiles);
  end;

  If MILanguages.Count> 0 then
    MILanguages.Enabled := True
  else
    MILanguages.Enabled := False;

  If SMILanguages.Count> 0 then
    SMILanguages.Enabled := True
  else
    SMILanguages.Enabled := False;

end;
function TFMain.SelectLanguageFlag(Language:String): Integer;
begin
  Result := 14;
  case Language of
    'EN' : Result := 15;
    'FR' : Result := 16;
    'ES' : Result := 17;
    'RU' : Result := 18;
    'ZH' : Result := 19;
    'JA' : Result := 20;
  end;
end;

procedure TFMain.ChangeLanguage(Sender: TObject);
begin
  SetDefaultLang(LowerCase(TMenuItem(Sender).Caption));
  MILanguages.Items[TMenuItem(Sender).MenuIndex].Checked  := True;
  SMILanguages.Items[TMenuItem(Sender).MenuIndex].Checked := True;
end;

end.

