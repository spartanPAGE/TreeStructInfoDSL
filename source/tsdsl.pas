unit tsdsl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes;

function Name(const Value: String): StrictName;
function Comment(const Value: String; const Delimeter: String = ''): StrictComment;
function Content(const Value: String): StrictContent;

function Attribute(const Name: StrictName): NamedAttribute; overload;
function Attribute(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedAttribute; overload;
function Attribute(
  const Name:    StrictName;
  const Comment: StrictComment;
  const Content: StrictContent
): NamedAttribute; overload;

function TreeStructInfo(const Name: StrictName): TTSInfoTree; overload;
function TreeStructInfo(
  const Name:    StrictName;
  const Comment: StrictComment
): TTSInfoTree; overload;
function TreeStructInfo(
  const Name:    StrictName;
  const Comment: StrictComment;
  const Nodes:   array of NamedNode
): TTSInfoTree; overload;
function TreeStructInfo(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Nodes:      array of NamedNode;
  const Attributes: array of NamedAttribute
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

function Content(const Value: String): StrictContent;
begin
  Content.Value := Value;
end;

function Attribute(const Name: StrictName): NamedAttribute;
begin
  Attribute.Name := Name;
end;

function Attribute(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedAttribute;
begin
  Attribute := Attribute(Name);
  Attribute.Comment := Comment;
end;

function Attribute(
  const Name:    StrictName;
  const Comment: StrictComment;
  const Content: StrictContent
): NamedAttribute;
begin
  Attribute := Attribute(Name, Comment);
  Attribute.Content := Content;
end;

function TreeStructInfo(const Name: StrictName): TTSInfoTree;
begin
  TreeStructInfo := TTSInfoTree.Create;
  TreeStructInfo.RenameTree(Name.Value);
end;

function TreeStructInfo(
  const Name:    StrictName;
  const Comment: StrictComment
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name);
  TreeStructInfo.WriteTreeComment(Comment.Value, '');
end;

function TreeStructInfo(
  const Name:    StrictName;
  const Comment: StrictComment;
  const Nodes:   array of NamedNode
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name, Comment);
  //todo: use nodes
end;

function TreeStructInfo(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Nodes:      array of NamedNode;
  const Attributes: array of NamedAttribute
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name, Comment, Nodes);
  //todo: use attributes
end;

end.

