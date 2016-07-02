unit tsdsl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes;

type StrictName = record
  Value: String;
end;

type StrictComment = record
  Value: String;
  Delimeter: String;
end;

function Name(const Value: String): StrictName;

function Comment(const Value: String; const Delimeter: String = ''): StrictComment;

function TreeStructInfo(const Name: StrictName): TTSInfoTree; overload;
function TreeStructInfo(
  const Name: StrictName;
  const Comment: StrictComment
): TTSInfoTree; overload;

implementation

function Name(const Value: String): StrictName;
begin
  Name.Value := Value;
end;

function Comment(const Value: String; const Delimeter: String): StrictComment;
begin
  Comment.Value := Value;
  Comment.Delimeter := Delimeter;
end;

function TreeStructInfo(const Name: StrictName): TTSInfoTree;
begin
  TreeStructInfo := TTSInfoTree.Create;
  TreeStructInfo.RenameTree(Name.Value);
end;

function TreeStructInfo(
  const Name: StrictName;
  const Comment: StrictComment
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name);
  TreeStructInfo.WriteTreeComment(Comment.Value, '');
end;

end.

