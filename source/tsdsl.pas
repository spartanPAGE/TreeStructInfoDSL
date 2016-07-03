unit tsdsl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes, TSDSLHelpers;

function Name(const Value: String): StrictName;
function Comment(const Value: String; const Delimeter: String = ''): StrictComment;
function Content(const Value: String): StrictContent;

function DeclarationComment(const Value: String; const Delimeter: String = ''): StrictDeclarationComment;
function DefinitionComment(const Value: String; const Delimeter: String = ''): StrictDefinitionComment;

function Attribute(const Name: StrictName): NamedAttribute;
function Attribute(
  const Name:    StrictName;
  const Content: StrictContent
): NamedAttribute;
function Attribute(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedAttribute;
function Attribute(
  const Name:    StrictName;
  const Comment: StrictComment;
  const Content: StrictContent
): NamedAttribute;

function RefAttribute(const Name: StrictName): NamedAttribute;
function RefAttribute(
  const Name:    StrictName;
  const Content: StrictContent
): NamedAttribute;
function RefAttribute(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment
): NamedAttribute;
function RefAttribute(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Content:            StrictContent
): NamedAttribute;
function RefAttribute(
  const Name:              StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment: StrictDefinitionComment
): NamedAttribute;
function RefAttribute(
  const Name:              StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment: StrictDefinitionComment;
  const Content:            StrictContent
): NamedAttribute;

function Attributes(const OpenArray: array of NamedAttribute): NamedAttributes;


function Node(const Name: StrictName): NamedNode;
function Node(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedNode;
function Node(
  const Name: StrictName;
  const Children: NamedNodes
): NamedNode;
function Node(
  const Name: StrictName;
  const Attributes: NamedAttributes
): NamedNode;
function Node(
  const Name:     StrictName;
  const Comment:  StrictComment;
  const Children: NamedNodes
): NamedNode;
function Node(
  const Name:     StrictName;
  const Comment:  StrictComment;
  const Attributes: NamedAttributes
): NamedNode;
function Node(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Children:   NamedNodes;
  const Attributes: NamedAttributes
): NamedNode;

function RefNode(const Name: StrictName): NamedNode;
function RefNode(
  const Name:     StrictName;
  const Children: NamedNodes
): NamedNode;
function RefNode(
  const Name:       StrictName;
  const Attributes: NamedAttributes
  ): NamedNode;
function RefNode(
  const Name:       StrictName;
  const Children:   NamedNodes;
  const Attributes: NamedAttributes
  ): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment
): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Children:           NamedNodes
  ): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Attributes:         NamedAttributes
  ): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Children:           NamedNodes;
  const Attributes:         NamedAttributes
  ): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment
): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment;
  const Children: NamedNodes
): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment;
  const Attributes:         NamedAttributes
): NamedNode;
function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment;
  const Children:           NamedNodes;
  const Attributes:         NamedAttributes
): NamedNode;

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

function DeclarationComment(const Value: String; const Delimeter: String): StrictDeclarationComment;
begin
  DeclarationComment.Value     := Value;
  DeclarationComment.Delimeter := Delimeter;
end;

function DefinitionComment(const Value: String; const Delimeter: String): StrictDefinitionComment;
begin
  DefinitionComment.Value     := Value;
  DefinitionComment.Delimeter := Delimeter;
end;

function Attribute(const Name: StrictName): NamedAttribute;
begin
  Attribute.IsRef := False;
  Attribute.Name  := Name;
end;

function Attribute(
  const Name:    StrictName;
  const Content: StrictContent
): NamedAttribute;
begin
  Attribute         := Attribute(Name);
  Attribute.Content := Content;
end;

function Attribute(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedAttribute;
begin
  Attribute := Attribute(Name);
  Attribute.Comment.Declaration.Value := Comment.Value;
  Attribute.Comment.Delimeter         := Comment.Delimeter;
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

function RefAttribute(const Name: StrictName): NamedAttribute;
begin
  RefAttribute.IsRef := True;
  RefAttribute.Name  := Name;
end;

function RefAttribute(
  const Name:    StrictName;
  const Content: StrictContent
): NamedAttribute;
begin
  RefAttribute         := RefAttribute(Name);
  RefAttribute.Content := Content;
end;

function RefAttribute(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment
): NamedAttribute;
begin
  RefAttribute                     := RefAttribute(Name);
  RefAttribute.Comment.Declaration := DeclarationComment;
end;

function RefAttribute(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Content:            StrictContent
): NamedAttribute;
begin
  RefAttribute         := RefAttribute(Name, DeclarationComment);
  RefAttribute.Content := Content;
end;

function RefAttribute(
  const Name:              StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment: StrictDefinitionComment
): NamedAttribute;
begin
  RefAttribute                    := RefAttribute(Name, DeclarationComment);
  RefAttribute.Comment.Definition := DefinitionComment;
end;

function RefAttribute(
  const Name:              StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment: StrictDefinitionComment;
  const Content:            StrictContent
): NamedAttribute;
begin
  RefAttribute         := RefAttribute(Name, DeclarationComment, DefinitionComment);
  RefAttribute.Content := Content;
end;

function Attributes(const OpenArray: array of NamedAttribute): NamedAttributes;
var Index: Integer;
begin
  SetLength(Attributes, Length(OpenArray));
  for Index := 0 to High(OpenArray) do
    Attributes[Index] := OpenArray[Index];
end;


function Node(const Name: StrictName): NamedNode;
begin
  Node.IsRef := False;
  Node.Name  := Name;
end;

function Node(
  const Name:    StrictName;
  const Comment: StrictComment
): NamedNode;
begin
  Node := Node(Name);
  Node.Comment.Declaration.Value := Comment.Value;
  Node.Comment.Delimeter         := Comment.Delimeter;
end;

function Node(
  const Name: StrictName;
  const Children: NamedNodes
): NamedNode;
begin
  Node          := Node(Name);
  Node.Children := Children;
end;

function Node(
  const Name: StrictName;
  const Attributes: NamedAttributes
): NamedNode;
begin
  Node            := Node(Name);
  Node.Attributes := Attributes;
end;

function Node(
  const Name:     StrictName;
  const Comment:  StrictComment;
  const Children: NamedNodes
): NamedNode;
begin
  Node          := Node(Name, Comment);
  Node.Children := Children;
end;

function Node(
  const Name:     StrictName;
  const Comment:  StrictComment;
  const Attributes: NamedAttributes
): NamedNode;
begin
  Node            := Node(Name, Comment);
  Node.Attributes := Attributes;
end;

function Node(
  const Name:       StrictName;
  const Comment:    StrictComment;
  const Children:   NamedNodes;
  const Attributes: NamedAttributes
): NamedNode;
begin
  Node            := Node(Name, Comment, Children);
  Node.Attributes := Attributes;
end;

function RefNode(const Name: StrictName): NamedNode;
begin
  RefNode.IsRef := True;
  RefNode.Name  := Name;
end;

function RefNode(
  const Name:     StrictName;
  const Children: NamedNodes
  ): NamedNode;
begin
  RefNode          := RefNode(Name);
  RefNode.Children := Children;
end;

function RefNode(
  const Name:       StrictName;
  const Attributes: NamedAttributes
  ): NamedNode;
begin
  RefNode            := RefNode(Name);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:       StrictName;
  const Children:   NamedNodes;
  const Attributes: NamedAttributes
  ): NamedNode;
begin
  RefNode            := RefNode(Name, Children);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment
): NamedNode;
begin
  RefNode                     := RefNode(Name);
  RefNode.Comment.Declaration := DeclarationComment;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Children:           NamedNodes
  ): NamedNode;
begin
  RefNode := RefNode(Name, DeclarationComment);
  RefNode.Children := Children;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Attributes:         NamedAttributes
  ): NamedNode;
begin
  RefNode            := RefNode(Name, DeclarationComment);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const Children:           NamedNodes;
  const Attributes:         NamedAttributes
  ): NamedNode;
begin
  RefNode            := RefNode(Name, DeclarationComment, Children);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment
): NamedNode;
begin
  RefNode                    := RefNode(Name, DeclarationComment);
  RefNode.Comment.Definition := DefinitionComment;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment;
  const Children: NamedNodes
): NamedNode;
begin
  RefNode          := RefNode(Name, DeclarationComment, DefinitionComment);
  RefNode.Children := Children;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment;
  const Attributes:         NamedAttributes
): NamedNode;
begin
  RefNode            := RefNode(Name, DeclarationComment, DefinitionComment);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               StrictName;
  const DeclarationComment: StrictDeclarationComment;
  const DefinitionComment:  StrictDefinitionComment;
  const Children:           NamedNodes;
  const Attributes:         NamedAttributes
): NamedNode;
begin
  RefNode := RefNode(Name, DeclarationComment, DefinitionComment, Children);
  RefNode.Attributes := Attributes;
end;

function Nodes(const OpenArray: array of NamedNode): NamedNodes;
var Index: Integer;
begin
  SetLength(Nodes, Length(OpenArray));
  for Index := 0 to High(OpenArray) do
    Nodes[Index] := OpenArray[Index];
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

