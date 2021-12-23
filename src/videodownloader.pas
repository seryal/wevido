unit videodownloader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, regexpr, fpjson,
  jsonparser, Generics.Collections,
  system.NetEncoding;

type
  //TVideoQuality = (vqMedium, vqHd720, vqHd2160, vqHd1440, vqHd1080, vqLarge, vqSmall, vqTiny);

  // Download Begin
  TOnBeginDownload = procedure(AFilePath: string; ASize: int64) of object;
  // Download Progress
  TOnProgressDownload = procedure(AFilePath: string; AProgress, ASize: int64) of object;
  // Download End
  TOnEndDownload = TOnBeginDownload;
  // Download Error
  TOnError = procedure(Error: string; Code: integer) of object;


  TDownloadUrl = record
    itag: integer; // https://gist.github.com/sidneys/7095afe4da4ae58694d128b1034e01e2
    Link: string;
    mimeType: string;
    Quality: string;
    QualityLabel: string;
  end;
  TDownloadUrlList = specialize TList<TDownLoadUrl>;

  { TVideoInfo }

  TVideoInfo = class
  private
    FVideoName: string;
    FQuality: string;
    FAuthor: string;
    FLength: int64;
    FDownloadUrls: TDownloadUrlList;
    function DecodeText(AString: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    property VideoName: string read FVideoName;
    property Quality: string read FQuality;
    property Author: string read FAuthor;
    property Length: int64 read FLength;
    property DownloadUrls: TDownloadUrlList read FDownloadUrls;
  end;


  IDownLoader = interface
    procedure SetOnDataReceived(AEvent: TOnProgressDownload);
    function GetOnDataReceived: TOnProgressDownload;
    function SetWebURL(AWebUrl: string): string; virtual;
    function GetInfo: TVideoInfo; virtual;
    function DownLoad(AFileName: string): int64;
    function DownLoad(AUrl, AFileName: string): int64;
    property OnDataReceived: TOnProgressDownload
      read GetOnDataReceived write SetOnDataReceived;
  end;

  { TBaseDownloader }

  TBaseDownloader = class(TInterfacedObject, IDownLoader)
  private
    FVideoInfo: TVideoInfo;
    FWebUrl: string;
    FDownloadUrl: string;
    FOnDataReceived: TOnProgressDownload;
    procedure DataReceivedHandler(Sender: TObject;
      const ContentLength, CurrentPos: int64);
    function FindContent(ARegExpression, AText: string): string;
    procedure SetOnDataReceived(AEvent: TOnProgressDownload);
    function GetOnDataReceived: TOnProgressDownload;
  public
    constructor Create;
    destructor Destroy; override;
    function SetWebURL(AWebUrl: string): string; virtual;
    function GetInfo: TVideoInfo; virtual;
    function DownLoad(AFileName: string): int64;
    function DownLoad(AUrl, AFileName: string): int64;
    property DefaultDownloadUrl: string read FDownloadUrl;
    property OnDataReceived: TOnProgressDownload
      read FOnDataReceived write FOnDataReceived;
  end;

  { TYouTubeVideo }

  TYouTubeVideo = class(TBaseDownloader, IDownLoader)
  private
    FJsonData: TJSONData;
  public
    function SetWebURL(AWebUrl: string): string; overload;
    function GetInfo: TVideoInfo; override;
    destructor Destroy; override;
  end;

  { TYapVideo }

  TYapVideo = class(TBaseDownloader, IDownLoader)
  private
    FJsonData: TJSONData;
  public
    function SetWebURL(AWebUrl: string): string; overload;
    function GetInfo: TVideoInfo; override;
    destructor Destroy; override;
  end;



implementation

{ TYapVideo }

function TYapVideo.SetWebURL(AWebUrl: string): string;
var
  http: TFPHTTPClient;
  str: TStringList;
  r: TRegExpr;
begin
  str := TStringList.Create;
  try
    http := TFPHTTPClient.Create(nil);
    try
      //WriteLn(AWebUrl);
      try
        http.Get(AWebUrl, str);
        http.ResponseHeaders.SaveToFile('header.txt');
      except
        on e: Exception do
          writeln(http.ResponseStatusCode);

      end;
    finally
      FreeAndNil(http);
    end;
    str.SaveToFile('html.html');
    FDownloadUrl := FindContent(
      '\<input type=\"text\" name=\"direct_link\" value=\"(.*?)\".*?\/\>', str.Text);
    FVideoInfo.FVideoName := FVideoInfo.DecodeText(
      FindContent('\<meta property="og:title" content="(.*?)\>', str.Text));
    FVideoInfo.FAuthor := '';
  finally
    FreeAndNil(str);
  end;
end;

function TYapVideo.GetInfo: TVideoInfo;
begin
  Result := inherited GetInfo;
end;

destructor TYapVideo.Destroy;
begin
  inherited Destroy;
end;

{ TVideoInfo }

function TVideoInfo.DecodeText(AString: string): string;
var
  html: THTMLEncoding;
begin
  Result := AString;
  html := THTMLEncoding.Create;
  try
    Result := html.Decode(AString);
  finally
    FreeAndNil(html);
  end;
end;

constructor TVideoInfo.Create;
begin
  FDownloadUrls := TDownloadUrlList.Create;
end;

destructor TVideoInfo.Destroy;
begin
  FreeAndNil(FDownloadUrls);
  inherited Destroy;
end;

{ TBaseDownloader }

procedure TBaseDownloader.DataReceivedHandler(Sender: TObject;
  const ContentLength, CurrentPos: int64);
begin
  if Assigned(OnDataReceived) then
    OnDataReceived('', CurrentPos, ContentLength);
end;

function TBaseDownloader.FindContent(ARegExpression, AText: string): string;
var
  r: TRegExpr;
begin
  Result := '';
  r := TRegExpr.Create(ARegExpression);
  try
    if r.Exec(AText) then
      Result := r.Match[1]
    else
      exit;   // raise Exception
  finally
    FreeAndNil(r);
  end;
end;

procedure TBaseDownloader.SetOnDataReceived(AEvent: TOnProgressDownload);
begin
  FOnDataReceived := AEvent;
end;

function TBaseDownloader.GetOnDataReceived: TOnProgressDownload;
begin
  Result := FOnDataReceived;
end;

constructor TBaseDownloader.Create;
begin
  FVideoInfo := TVideoInfo.Create;
end;

destructor TBaseDownloader.Destroy;
begin
  FreeAndNil(FVideoInfo);
  inherited Destroy;
end;

function TBaseDownloader.SetWebURL(AWebUrl: string): string;
begin
  raise Exception.Create('Override GetDownloadUrl method');
end;

function TBaseDownloader.GetInfo: TVideoInfo;
begin
  Result := FVideoInfo;
end;

function TBaseDownloader.DownLoad(AFileName: string): int64;
begin
  DownLoad(FDownloadUrl, AFileName);
end;

function TBaseDownloader.DownLoad(AUrl, AFileName: string): int64;
var
  http: TFPHTTPClient;
  header: TStringList;
  size: int64;
  str: string;
  tmpfile: TFileStream;
begin
  http := TFPHTTPClient.Create(nil);
  try
    // https://www.yaplakal.com/forum28/topic2369414.html - html
    // https://www.yaplakal.com/forum28/topic2368974.html - video
    tmpfile := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
    try
      http.AllowRedirect := True;
      http.OnDataReceived := @DataReceivedHandler;
      http.Get(AUrl, tmpfile);
    finally
      FreeAndNil(tmpfile);
    end;
  finally
    FreeAndNil(http);
  end;
end;

{ TYouTubeVideo }

function TYouTubeVideo.SetWebURL(AWebUrl: string): string;
var
  http: TFPHTTPClient;
  str: TStringList;
  r: TRegExpr;
  res: TStringList;
  jData: TJSONData;
  jsonstr: string;
begin
  FWebUrl := AWebUrl;
  str := TStringList.Create;
  try
    http := TFPHTTPClient.Create(nil);
    try
      http.Get(AWebUrl, str);
    finally
      FreeAndNil(http);
    end;
    jsonstr := FindContent('var ytInitialPlayerResponse = (.*?);</script>', str.Text);
  finally
    FreeAndNil(str);
  end;

  try
    FJsonData := GetJSON(jsonstr);
    FDownloadUrl := TJSONObject(TJSONArray(FJsonData.FindPath('streamingData.formats'))
      [0]).Get('url');

    Result := FDownloadUrl;
  except
    on e: Exception do
      Result := '';
  end;
end;

function TYouTubeVideo.GetInfo: TVideoInfo;
var
  i: integer;
  jObj: TJSONObject;
  url: TDownloadUrl;
begin
  Result := inherited GetInfo;
  FVideoInfo.FVideoName := TJSONObject(FJsonData.FindPath('videoDetails')).Get('title', '');
  FVideoInfo.FAuthor := TJSONObject(FJsonData.FindPath('videoDetails')).Get('author', '');
  FVideoInfo.FLength := string(TJSONObject(FJsonData.FindPath('videoDetails')).Get('lengthSeconds', '-1')).ToInt64;
  i := TJSONArray(FJsonData.FindPath('streamingData.adaptiveFormats')).Count;
  for i := 0 to (FJsonData.FindPath('streamingData.adaptiveFormats') as
      TJSONArray).Count - 1 do
  begin
    jObj := (FJsonData.FindPath('streamingData.adaptiveFormats') as TJSONArray)[i] as
      TJSONObject;
    url.itag := jObj.Get('itag');
    url.Link := jObj.Get('url');
    url.mimeType := jObj.Get('mimeType');
    url.Quality := jObj.Get('quality', '');
    url.QualityLabel := jObj.Get('qualityLabel', '');

    FVideoInfo.FDownloadUrls.Add(url);
  end;
end;

destructor TYouTubeVideo.Destroy;
begin
  if Assigned(FJsonData) then
    FreeAndNil(FJsonData);
  inherited Destroy;
end;

end.
