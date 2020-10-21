# ksS3
Simple Delphi interface for Amazon S3 Storage service.

It's not complete, I'm just adding to it as I need it for one of my projects.  If you'd like me to add support for particular functionality, let me know ;-) It should be fairly easy to extend the interface and add more functionality.

I'll be adding in some code I've written in another project which deals with the uploading of objects.

Note. This is currently for XE8 and newer as it relies on the Net.UrlClient and Net.HttpClient units.  I'm sure I can make it compatible with Indy however if there's enough interest.

#### Example use

There's an example project included which lists buckets, objects and object infomration. 

The below code will populate a Listbox with the list of buckets in your S3 service.

```
uses ksAwsS3
...
procedure TForm10.Button1Click(Sender: TObject);
var
  AAwsS3: IksAwsS3;
begin
  AAwsS3 := CreateAwsS3(YOUR_PUBLIC_KEY, YOUR_SECRET_KEY, s3EuWest1);

  AAwsS3.GetBuckets(ListBox1.Items);
end;




 ```
