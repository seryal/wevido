program youtube_download;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,                                              {$ENDIF}
  Classes,
  SysUtils,
  videodownloader,
  CustApp,
  crt;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  private
    Fx, Fy: tcrtcoord;
    procedure RecvHandler(AFilePath: string; AProgress, ASize: int64);
  protected
    FDownloader: IDownLoader;
    procedure DoRun; override;
    function CreateDownloader(AUrl: string): IDownLoader;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMyApplication }

  procedure TMyApplication.RecvHandler(AFilePath: string; AProgress, ASize: int64);
  begin
    if ASize = 0 then
      exit;
    GotoXY(fx, fy);
    WriteLn(trim(FormatFloat('### ### ### ### ###', AProgress)), '/', trim(FormatFloat('### ### ### ### ###', ASize)),
      '              ');
  end;

  procedure TMyApplication.DoRun;
  var
    ErrorMsg: string;
    url: string;
    filename: TFileName;
    info: TVideoInfo;
    i: integer;
    downloadurl: TDownloadUrl;
  begin
    DefaultSystemCodePage := CP_UTF8;
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;
    url := ParamStr(1);
    if ParamStr(2) <> '' then
      filename := ParamStr(2)
    else
      filename := 'downloaded.mp4';

    writeln(url);
    FDownloader := CreateDownloader(url);
    if not Assigned(FDownloader) then
    begin
      writeln('Resource not found');
      exit;
    end;

    FDownloader.OnDataReceived := @RecvHandler;
    FDownloader.SetWebURL(url);
    info := FDownloader.GetInfo;
    writeln('Title - ', info.VideoName);
    writeln('Author - ', info.Author);
    writeln('Length - ', info.Length, ' seconds');
    for downloadurl in info.DownloadUrls do
    begin
      TextColor(Yellow);
      Write(' - ', downloadurl.mimeType, ': ');
      TextColor(LightCyan);
      writeln(downloadurl.quality, ' - ', downloadurl.QualityLabel);
      TextColor(LightGray);
      writeln('"', downloadurl.Link, '"');

    end;
    writeln('----');
    writeln('Download default URL.');
    writeln('----');
    writeln('');

    fx := WhereX;
    fy := WhereY - 1;

    FDownloader.DownLoad(filename);
    WriteLn('Download complete');

    { add your program here }

    // stop program loop
    // readln;
    Terminate;
  end;

  function TMyApplication.CreateDownloader(AUrl: string): IDownLoader;
  begin
    Result := nil;
    if pos('youtube.com', AUrl) <> 0 then
      Result := TYouTubeVideo.Create
    else
    if (pos('yaplakal.com', AUrl) <> 0) or (pos('yap.ru', AUrl) <> 0) then
      Result := TYapVideo.Create;
  end;

  constructor TMyApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMyApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMyApplication.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.
