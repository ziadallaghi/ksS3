{*******************************************************************************
*                                                                              *
*  ksAwsSes - Amazon S3 Interface for Delphi                                   *
*                                                                              *
*  https://github.com/gmurt/ksStripe                                           *
*                                                                              *
*  Copyright 2020 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksAwsS3;

interface

uses Classes;


type
  TksS3Region = (s3EuCentral1, s3EuNorth1, s3EuSouth1, s3EuWest1, s3EuWest2, s3EuWest3, s3UsEast1, s3UsEast2, s3UsWest1, s3UsWest2);

  IksAwsS3Object = interface
    ['{C9390D73-62A6-43F1-AAE1-479693BAAAFC}']
    function GetKey: string;
    function GetObjectName: string;
    function GetStream: TStream;
    function GetSize: integer;
    function GetETag: string;
    function GetLastModified: string;
    procedure SaveToFile(AFilename: string);
    property Key: string read GetKey;
    property ObjectName: string read GetObjectName;
    property Stream: TStream read GetStream;
    property Size: integer read GetSize;
    property ETag: string read GetETag;
    property LastModified: string read GetLastModified;
  end;

  IksAwsS3 = interface
    ['{BD814B29-8F03-425F-BF47-FECBEA49D133}']
    function GetObject(ABucketName, AObjectName: string): IksAwsS3Object;
    procedure GetBuckets(ABuckets: TStrings);
    procedure GetBucket(ABucketName: string; AContents: TStrings);
  end;

  function CreateAwsS3(APublicKey, APrivateKey: string; ARegion: TksS3Region): IksAwsS3;

implementation

uses SysUtils, System.DateUtils, Net.UrlClient, Net.HttpClient, System.Hash, HttpApp,
  System.NetEncoding, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc;

const
  C_EMPTY_HASH        = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855';
  C_SERVICE           = 's3';
  C_LF                = #10;
  C_AMZ_DATE_FORMAT   = 'yyyymmdd"T"hhnnss"Z"';
  C_SHORT_DATE_FORMAT = 'yyyymmdd';
  C_AMAZON_DOMAIN     = 'amazonaws.com';
  C_HASH_ALGORITHM    = 'AWS4-HMAC-SHA256';
  C_PROTOCOL          = 'https';
  C_S3_RGN_EU_CENTRAL_1   = 'eu-central-1';
  C_S3_RGN_EU_NORTH_1     = 'eu-north-1';
  C_S3_RGN_EU_SOUTH_1     = 'eu-south-1';
  C_S3_RGN_EU_WEST_1      = 'eu-west-1';
  C_S3_RGN_EU_WEST_2      = 'eu-west-2';
  C_S3_RGN_EU_WEST_3      = 'eu-west-3';
  C_S3_RGN_US_EAST_1      = 'us-east-1';
  C_S3_RGN_US_EAST_2      = 'us-east-2';
  C_S3_RGN_US_WEST_1      = 'us-west-1';
  C_S3_RGN_US_WEST_2      = 'us-west-2';

type
  TksAwsS3Object = class(TInterfacedObject, IksAwsS3Object)
  private
    FKey: string;
    FObjectName: string;
    FEtag: string;
    FStream: TStream;
    FLastModified: string;
    function GetETag: string;
    function GetStream: TStream;
    function GetSize: integer;
    function GetKey: string;
    function GetObjectName: string;
    function GetLastModified: string;
  protected
    procedure SaveToFile(AFilename: string);

    property Key: string read GetKey;
    property ObjectName: string read GetObjectName;
    property ETag: string read GetETag;
    property LastModified: string read GetLastModified;
    property Size: integer read GetSize;
    property Stream: TStream read GetStream;


  public
    constructor Create(AKey: string;
                       AObjectName: string;
                       AETag: string;
                       ALastModified: string;
                       AStream: TStream); virtual;
    destructor Destroy; override;
  end;

  TksAwsS3 = class(TInterfacedObject, IksAwsS3)
  private
    FPublicKey: string;
    FPrivateKey: string;
    FRegion: TksS3Region;
    //function GetHost: string;
    procedure DoValidateCert(const Sender: TObject; const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
    function GetRegionStr(ARegion: TksS3Region): string;
    function CalculateHMACSHA256(const AValue: string; const AKey: TArray<Byte>): TArray<Byte>;
    function CalculateHMACSHA256Hex(const AValue: string; const AKey: TArray<Byte>): string;
    function GetHashSHA256Hex(HashString: string): string;
    function GenerateCanonicalRequest(AHost, ADate, AObj: string): string;
    function GenerateSignature(AStrToSign, ADateStr: string): string;
    function PerformGetRequest(ABucket, AObj: string; const AStream: TStream = nil) : IHttpResponse;
    function GenerateUrl(ABucket, AObj: string): string;
    function GetHost(ABucket: string): string;
  protected
    function GetObject(ABucketName, AObjectName: string): IksAwsS3Object;
    procedure GetBuckets(AStrings: TStrings);
    procedure GetBucket(ABucketName: string; AStrings: TStrings);
  public
    constructor Create(APublicKey, APrivateKey: string; ARegion: TksS3Region);
  end;


function CreateAwsS3(APublicKey, APrivateKey: string; ARegion: TksS3Region): IksAwsS3;
begin
  Result := TksAwsS3.Create(APublicKey, APrivateKey, ARegion);
end;

{ TksAwsS3Object }

constructor TksAwsS3Object.Create(AKey: string;
                                  AObjectName: string;
                                  AETag: string;
                                  ALastModified: string;
                                  AStream: TStream);
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FStream.CopyFrom(AStream, AStream.Size);
  FStream.Position := 0;
  FEtag := AETag;
  FKey := AKey;
  FObjectName := AObjectName;
  FLastModified := ALastModified;
end;

destructor TksAwsS3Object.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TksAwsS3Object.GetETag: string;
begin
  Result := FEtag;
end;

function TksAwsS3Object.GetKey: string;
begin
  Result := FLastModified;
end;

function TksAwsS3Object.GetLastModified: string;
begin
  Result := FLastModified;
end;

function TksAwsS3Object.GetObjectName: string;
begin
  Result := FObjectName;
end;

function TksAwsS3Object.GetSize: integer;
begin
  Result := FStream.Size;
end;

function TksAwsS3Object.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TksAwsS3Object.SaveToFile(AFilename: string);
begin
  (FStream as TMemoryStream).SaveToFile(AFilename);
end;

{ TksAwsS3 }

constructor TksAwsS3.Create(APublicKey, APrivateKey: string; ARegion: TksS3Region);
begin
  inherited Create;
  FPublicKey := APublicKey;
  FPrivateKey := APrivateKey;
  FRegion := ARegion;
end;

procedure TksAwsS3.DoValidateCert(const Sender: TObject; const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
begin
  Accepted := True;
end;

procedure TksAwsS3.GetBucket(ABucketName: string; AStrings: TStrings);
var
  AResponse: string;
  AXml: IXMLDocument;
  AContents: IXMLNode;
  AObject: IXMLNode;
  ICount: integer;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    AResponse := PerformGetRequest(ABucketName, '').ContentAsString;
    AXml := TXMLDocument.Create(nil);
    AXml.LoadFromXML(AResponse);
    AContents := AXml.ChildNodes['ListBucketResult'];
    for ICount := 0 to AContents.ChildNodes.Count-1 do
    begin
      AObject := AContents.ChildNodes[ICount];
      if AObject.NodeName = 'Contents' then
        AStrings.Add(AObject.ChildValues['Key']);
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TksAwsS3.GetBuckets(AStrings: TStrings);
var
  AResponse: string;
  AXml: IXMLDocument;
  ABuckets: IXMLNode;
  ABucket: IXMLNode;
  ICount: integer;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    AResponse := PerformGetRequest('', '').ContentAsString;
    AXml := TXMLDocument.Create(nil);
    AXml.LoadFromXML(AResponse);
    ABuckets := AXml.ChildNodes['ListAllMyBucketsResult'];
    ABuckets := ABuckets.ChildNodes['Buckets'];
    for ICount := 0 to ABuckets.ChildNodes.Count-1 do
    begin
      ABucket := ABuckets.ChildNodes[ICount];
      AStrings.Add(ABucket.ChildNodes['Name'].Text);
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

function TksAwsS3.GetObject(ABucketName, AObjectName: string): IksAwsS3Object;
var
  AResponse: IHTTPResponse;
  AStream: TStream;
  AFilename: string;
begin
  AStream := TMemoryStream.Create;
  try
    AResponse := PerformGetRequest(ABucketName, AObjectName, AStream);
    AFilename := AObjectName;
    while Pos('/', AFilename) > 0 do
      AFilename := Copy(AFilename, Pos('/', AFilename)+1, Length(AFilename));
    Result := TksAwsS3Object.Create(AFilename,
                                    AObjectName,
                                    AResponse.HeaderValue['ETag'],
                                    AResponse.LastModified,
                                    AStream);
  finally
    AStream.Free;
  end;
end;

function TksAwsS3.GetHashSHA256Hex( HashString: string): string;
begin
  Result := THash.DigestAsString(THashSHA2.GetHashBytes(HashString));
end;

function TksAwsS3.GetHost(ABucket: string): string;
begin
  Result := Format('%s.%s.%s', [C_SERVICE, GetRegionStr(FRegion), C_AMAZON_DOMAIN]);
  if ABucket <> '' then
    Result := ABucket+'.'+Result;
end;

function TksAwsS3.GetRegionStr(ARegion: TksS3Region): string;
begin
  case ARegion of
    s3EuCentral1: Result := C_S3_RGN_EU_CENTRAL_1;
    s3EuNorth1  : Result := C_S3_RGN_EU_NORTH_1;
    s3EuSouth1  : Result := C_S3_RGN_EU_SOUTH_1;
    s3EuWest1   : Result := C_S3_RGN_EU_WEST_1;
    s3EuWest2   : Result := C_S3_RGN_EU_WEST_2;
    s3EuWest3   : Result := C_S3_RGN_EU_WEST_3;
    s3UsEast1   : Result := C_S3_RGN_US_EAST_1;
    s3UsEast2   : Result := C_S3_RGN_US_EAST_2;
    s3UsWest1   : Result := C_S3_RGN_US_WEST_1;
    s3UsWest2   : Result := C_S3_RGN_US_WEST_2;
  end;
end;

function TksAwsS3.CalculateHMACSHA256(const AValue: string; const AKey: TArray<Byte>): TArray<Byte>;
begin
  Result := THashSHA2.GetHMACAsBytes(AValue, AKey);
end;

function TksAwsS3.CalculateHMACSHA256Hex(const AValue: string; const AKey: TArray<Byte>): string;
begin
  Result := lowercase(THash.DigestAsString(CalculateHMACSHA256(AValue, AKey)));
end;

function TksAwsS3.GenerateCanonicalRequest(AHost, ADate, AObj: string): string;
begin
  Result := TNetEncoding.URL.Encode('GET') +C_LF;
  Result := Result + '/' + TNetEncoding.URL.Encode(AObj, [Ord('#')], []) +C_LF+C_LF;
  Result := Result + 'host:' + Trim(TNetEncoding.URL.Encode(AHost)) +C_LF+
                     'x-amz-content-sha256:' + C_EMPTY_HASH +C_LF+
                     'x-amz-date:' + Trim(ADate) +C_LF;
  Result := Result + C_LF+'host;x-amz-content-sha256;x-amz-date' +C_LF;
  Result := Result + C_EMPTY_HASH;
end;

function TksAwsS3.GenerateSignature(AStrToSign, ADateStr: string): string;
var
  ADateKey, ARegionKey, AServiceKey, ASigningKey: TArray<Byte>;
begin
  ADateKey := CalculateHMACSHA256(ADateStr, TEncoding.UTF8.GetBytes('AWS4' + FPrivateKey));
  ARegionKey := CalculateHMACSHA256(GetRegionStr(FRegion), ADateKey);
  AServiceKey := CalculateHMACSHA256(C_SERVICE, ARegionKey);
  ASigningKey := CalculateHMACSHA256('aws4_request', AServiceKey);
  Result := CalculateHMACSHA256Hex(AStrToSign, ASigningKey);
end;

function TksAwsS3.GenerateUrl(ABucket, AObj: string): string;
begin
  Result := C_PROTOCOL+'://';
  if ABucket <> '' then Result := Result + TNetEncoding.URL.Encode(ABucket)+'.';
  Result := Result + C_SERVICE+'.'+GetRegionStr(FRegion)+'.'+C_AMAZON_DOMAIN+'/';
  if AObj <> '' then
    Result := Result + TNetEncoding.URL.Encode(AObj, [Ord('#')], []);
end;

function TksAwsS3.PerformGetRequest(ABucket, AObj: string; const AStream: TStream = nil): IHttpResponse;
var
  AAmzDate: string;
  AUrl: string;
  StringToSign: string;
  Signature: string;
  AuthorisationHeader: string;
  ARequestTime: TDateTime;
  AHttp: THTTPClient;
  ACanonicalRequest: string;
  AShortDate: string;
begin
  ARequestTime := Now;
  AShortDate := FormatDateTime(C_SHORT_DATE_FORMAT, ARequestTime);
  AAmzDate := FormatDateTime(C_AMZ_DATE_FORMAT, TTimeZone.Local.ToUniversalTime(ARequestTime), TFormatSettings.Create('en-US'));
  AUrl := GenerateUrl(ABucket, AObj);
  ACanonicalRequest := GenerateCanonicalRequest(GetHost(ABucket), AAmzDate, AObj);
  StringToSign := C_HASH_ALGORITHM +C_LF
                  + AAmzDate +C_LF
                  + AShortDate +'/'+ GetRegionStr(FRegion) +'/'+ C_SERVICE +'/aws4_request' +C_LF
                  + GetHashSHA256Hex(ACanonicalRequest);
  Signature := GenerateSignature(StringToSign, AShortDate);
  AuthorisationHeader := C_HASH_ALGORITHM+' Credential='+FPublicKey+'/'+AShortDate+'/'+GetRegionStr(FRegion)+'/'+C_SERVICE+'/aws4_request,SignedHeaders=host;x-amz-content-sha256;x-amz-date,Signature='+signature;
  AHttp := THTTPClient.Create;
  try
    AHttp.CustomHeaders['Authorization'] := AuthorisationHeader;
    AHttp.CustomHeaders['x-amz-content-sha256'] := C_EMPTY_HASH;
    AHttp.CustomHeaders['x-amz-date'] := AAmzDate;
    AHttp.OnValidateServerCertificate := DoValidateCert;

    Result := AHttp.Get(AURL, AStream);
    if AStream <> nil then
      AStream.Position := 0;
  finally
    AHttp.Free;
  end;
end;

end.
