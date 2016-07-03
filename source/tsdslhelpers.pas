unit TSDSLHelpers;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes;


procedure AddNodesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Nodes: NamedNodes
);

procedure AddAttributesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Attrs: NamedAttributes
);

function GluePath(
  const Lhs:       String;
  const Rhs:       String;
  const Delimeter: String = '\'
): String;

implementation

procedure AddNodesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Nodes: NamedNodes
);
var NNode: NamedNode;
var GluedPath: String;
begin
  for NNode in Nodes do
  begin
    GluedPath := GluePath(Path, NNode.Name.Value);
    with Tree do
    begin
      CreateChildNode(Path, False, NNode.Name.Value);
      WriteChildNodeComment(
        GluedPath,
        NNode.Comment.DeclarationValue,
        NNode.Comment.Delimeter,
        ctDeclaration
      );
      WriteChildNodeComment(
        GluedPath,
        NNode.Comment.DefinitionValue,
        NNode.Comment.Delimeter,
        ctDefinition
      );
    end;
    AddAttributesToTree(Tree, GluedPath, NNode.Attributes);
  end;
end;

procedure AddAttributesToTree(
  var Tree:    TTSInfoTree;
  const Path:  String;
  const Attrs: NamedAttributes
);
var Attr: NamedAttribute;
var GluedPath: String;
begin
  for Attr in Attrs do
  begin
    GluedPath := GluePath(Path, Attr.Name.Value);
    with Tree do
    begin
      CreateAttribute(Path, False, Attr.Name.Value);
      WriteAttributeComment(
        GluedPath,
        Attr.Comment.DeclarationValue,
        Attr.Comment.Delimeter,
        ctDeclaration
      );
      WriteAttributeComment(
        GluedPath,
        Attr.Comment.DefinitionValue,
        Attr.Comment.Delimeter,
        ctDefinition
      );
      WriteString(GluedPath, Attr.Content.Value);
    end;
  end;
end;

function GluePath(
  const Lhs:       String;
  const Rhs:       String;
  const Delimeter: String
): String;
begin
  if Rhs = '' then
    GluePath := Lhs
  else if Lhs = '' then
    GluePath := Rhs
  else
    GluePath := Lhs + Delimeter + Rhs;
end;

end.

