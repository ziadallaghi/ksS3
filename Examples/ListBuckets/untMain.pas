unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, ksAwsS3;

type
  TForm10 = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    ListBox1: TListBox;
    Panel2: TPanel;
    ListBox3: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel3: TPanel;
    ListBox2: TListBox;
    Label3: TLabel;
    Panel4: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
  private
    FAwsS3: IksAwsS3;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses System.DateUtils, System.NetEncoding, System.Hash, Net.HttpClient;

const
  //  Note. You'll need to add your public/secret credentials in to the
  //  const values below...

  C_YOUR_PUBLIC_KEY = '********************';
  C_YOUR_SECRET_KEY = '****************************************';


{$R *.dfm}

procedure TForm10.Button1Click(Sender: TObject);
begin
  ListBox1.Items.Clear;
  ListBox2.Items.Clear;
  FAwsS3.GetBuckets(ListBox1.Items);
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FAwsS3 := CreateAwsS3(C_YOUR_PUBLIC_KEY, C_YOUR_SECRET_KEY, s3EuWest1);
end;

procedure TForm10.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
    FAwsS3.GetBucket(ListBox1.Items[ListBox1.ItemIndex], ListBox2.Items);
end;

procedure TForm10.ListBox2Click(Sender: TObject);
var
  ABucket: string;
  AObjectName: string;
  AObject: IksAwsS3Object;
begin
  ListBox3.Items.Clear;
  if ListBox1.ItemIndex > -1 then ABucket := ListBox1.Items[ListBox1.ItemIndex];
  if ListBox2.ItemIndex > -1 then AObjectName := ListBox2.Items[ListBox2.ItemIndex];

  AObject := FAwsS3.GetObject(ABucket, AObjectName);

  ListBox3.Items.Add('key: '+AObject.Key);
  ListBox3.Items.Add('object name: '+AObject.ObjectName);
  ListBox3.Items.Add('size: '+AObject.Size.ToString+' bytes');
  ListBox3.Items.Add('last modified: '+AObject.LastModified);
  ListBox3.Items.Add('ETag: '+AObject.ETag);
end;

end.
