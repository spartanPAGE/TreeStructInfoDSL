unit tsdsl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes, TSDSLHelpers;

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

function Attributes(const OpenArray: array of NamedAttribute): NamedAttributes;

function Node(const Name: StrictName): NamedNode;
function Node(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedNode; overload;
function Node(
  const Name:     StrictName;
  const Comment:  StrictComment;
  const Children: NamedNodes
): NamedNode; overload;
function Node(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Children:   NamedNodes;
  const Attributes: NamedAttributes
): NamedNode; overload;

function Nodes(const OpenArray: array of NamedNode): NamedNodes;


function TreeStructInfo(const Name: StrictName): TTSInfoTree; overload;
function TreeStructInfo(
  const Name:    StrictName;
  const Comment: StrictComment
): TTSInfoTree; overload;
function TreeStructInfo(
  const Name:    StrictName;
  const Comment: StrictComment;
  const Nodes:   NamedNodes
): TTSInfoTree; overload;
function TreeStructInfo(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Nodes:      NamedNodes;
  const Attributes: NamedAttributes
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

function Attributes(const OpenArray: array of NamedAttribute): NamedAttributes;
begin
  SetLength(Attributes, Length(OpenArray));
  Move(OpenArray[Low(OpenArray)], Attributes[0], Length(OpenArray)*SizeOf(NamedAttribute));
end;


function Node(const Name: StrictName): NamedNode;
begin
  Node.Name := Name;
end;

function Node(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedNode;
begin
  Node := Node(Name);
  Node.Comment := Comment;
end;

function Node(
  const Name:     StrictName;
  const Comment:  StrictComment;
  const Children: NamedNodes
): NamedNode;
begin
  Node := Node(Name, Comment);
  Node.Children := Children;
end;

function Node(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Children:   NamedNodes;
  const Attributes: NamedAttributes
): NamedNode;
begin
  Node := Node(Name, Comment, Children);
  Node.Attributes := Attributes;
end;

function Nodes(const OpenArray: array of NamedNode): NamedNodes;
begin
  SetLength(Nodes, Length(OpenArray));
  Move(OpenArray[Low(OpenArray)], Nodes[0], Length(OpenArray)*SizeOf(NamedNode));
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
  const Nodes:   NamedNodes
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name, Comment);
  TSDSLHelpers.AddNodesToTree(TreeStructInfo, '', Nodes);
end;

function TreeStructInfo(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Nodes:      NamedNodes;
  const Attributes: NamedAttributes
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name, Comment, Nodes);
  TSDSLHelpers.AddAttributesToTree(TreeStructInfo, '', Attributes);
end;

end.

