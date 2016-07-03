unit tsdsl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TSInfoFiles, TSInfoConsts, TSInfoTypes, TSDSLTypes, TSDSLHelpers;

function Name(const Value: String): TStrictName;
function Comment(const Value: String; const Delimeter: String = ''): TStrictComment;
function Content(const Value: String): TStrictContent;

function DeclarationComment(const Value: String; const Delimeter: String = ''): TStrictDeclarationComment;
function DefinitionComment(const Value: String; const Delimeter: String = ''): TStrictDefinitionComment;

function Attribute(const Name: TStrictName): TNamedAttribute;
function Attribute(
  const Name:    TStrictName;
  const Content: TStrictContent
): TNamedAttribute;
function Attribute(
  const Name:    TStrictName;
  const Comment: TStrictComment
): TNamedAttribute;
function Attribute(
  const Name:    TStrictName;
  const Comment: TStrictComment;
  const Content: TStrictContent
): TNamedAttribute;

function RefAttribute(const Name: TStrictName): TNamedAttribute;
function RefAttribute(
  const Name:    TStrictName;
  const Content: TStrictContent
): TNamedAttribute;
function RefAttribute(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment
): TNamedAttribute;
function RefAttribute(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Content:            TStrictContent
): TNamedAttribute;
function RefAttribute(
  const Name:              TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment: TStrictDefinitionComment
): TNamedAttribute;
function RefAttribute(
  const Name:              TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment: TStrictDefinitionComment;
  const Content:            TStrictContent
): TNamedAttribute;

function Attributes(const OpenArray: array of TNamedAttribute): NamedAttributes;


function Node(const Name: TStrictName): TNamedNode;
function Node(
  const Name:    TStrictName;
  const Comment: TStrictComment
): TNamedNode;
function Node(
  const Name: TStrictName;
  const Children: TNamedNodes
): TNamedNode;
function Node(
  const Name: TStrictName;
  const Attributes: NamedAttributes
): TNamedNode;
function Node(
  const Name: TStrictName;
  const Children: TNamedNodes;
  const Attributes: NamedAttributes
): TNamedNode;
function Node(
  const Name:     TStrictName;
  const Comment:  TStrictComment;
  const Children: TNamedNodes
): TNamedNode;
function Node(
  const Name:     TStrictName;
  const Comment:  TStrictComment;
  const Attributes: NamedAttributes
): TNamedNode;
function Node(
  const Name:       TStrictName;
  const Comment:    TStrictComment;
  const Children:   TNamedNodes;
  const Attributes: NamedAttributes
): TNamedNode;

function RefNode(const Name: TStrictName): TNamedNode;
function RefNode(
  const Name:     TStrictName;
  const Children: TNamedNodes
): TNamedNode;
function RefNode(
  const Name:       TStrictName;
  const Attributes: NamedAttributes
  ): TNamedNode;
function RefNode(
  const Name:       TStrictName;
  const Children:   TNamedNodes;
  const Attributes: NamedAttributes
  ): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment
): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Children:           TNamedNodes
  ): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Attributes:         NamedAttributes
  ): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Children:           TNamedNodes;
  const Attributes:         NamedAttributes
  ): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment
): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment;
  const Children: TNamedNodes
): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment;
  const Attributes:         NamedAttributes
): TNamedNode;
function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment;
  const Children:           TNamedNodes;
  const Attributes:         NamedAttributes
): TNamedNode;

function Nodes(const OpenArray: array of TNamedNode): TNamedNodes;


function TreeStructInfo(const Name: TStrictName): TTSInfoTree;
function TreeStructInfo(
  const Name:  TStrictName;
  const Nodes: TNamedNodes
): TTSINfoTree;
function TreeStructInfo(
  const Name:       TStrictName;
  const Attributes: NamedAttributes
): TTSINfoTree;
function TreeStructInfo(
  const Name:       TStrictName;
  const Nodes:      TNamedNodes;
  const Attributes: NamedAttributes
): TTSInfoTree;
function TreeStructInfo(
  const Name:    TStrictName;
  const Comment: TStrictComment
): TTSInfoTree;
function TreeStructInfo(
  const Name:    TStrictName;
  const Comment: TStrictComment;
  const Nodes:   TNamedNodes
): TTSInfoTree;
function TreeStructInfo(
  const Name:       TStrictName;
  const Comment:    TStrictComment;
  const Nodes:      TNamedNodes;
  const Attributes: NamedAttributes
): TTSInfoTree;

implementation

function Name(const Value: String): TStrictName;
begin
  Name.Value := Value;
end;

function Comment(const Value: String; const Delimeter: String): TStrictComment;
begin
  Comment.Value := Value;
  Comment.Delimeter := Delimeter;
end;

function Content(const Value: String): TStrictContent;
begin
  Content.Value := Value;
end;

function DeclarationComment(const Value: String; const Delimeter: String): TStrictDeclarationComment;
begin
  DeclarationComment.Value     := Value;
  DeclarationComment.Delimeter := Delimeter;
end;

function DefinitionComment(const Value: String; const Delimeter: String): TStrictDefinitionComment;
begin
  DefinitionComment.Value     := Value;
  DefinitionComment.Delimeter := Delimeter;
end;

function Attribute(const Name: TStrictName): TNamedAttribute;
begin
  Attribute.IsRef := False;
  Attribute.Name  := Name;
end;

function Attribute(
  const Name:    TStrictName;
  const Content: TStrictContent
): TNamedAttribute;
begin
  Attribute         := Attribute(Name);
  Attribute.Content := Content;
end;

function Attribute(
  const Name:    TStrictName;
  const Comment: TStrictComment
): TNamedAttribute;
begin
  Attribute := Attribute(Name);
  Attribute.Comment.Declaration.Value := Comment.Value;
  Attribute.Comment.Delimeter         := Comment.Delimeter;
end;

function Attribute(
  const Name:    TStrictName;
  const Comment: TStrictComment;
  const Content: TStrictContent
): TNamedAttribute;
begin
  Attribute := Attribute(Name, Comment);
  Attribute.Content := Content;
end;

function RefAttribute(const Name: TStrictName): TNamedAttribute;
begin
  RefAttribute.IsRef := True;
  RefAttribute.Name  := Name;
end;

function RefAttribute(
  const Name:    TStrictName;
  const Content: TStrictContent
): TNamedAttribute;
begin
  RefAttribute         := RefAttribute(Name);
  RefAttribute.Content := Content;
end;

function RefAttribute(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment
): TNamedAttribute;
begin
  RefAttribute                     := RefAttribute(Name);
  RefAttribute.Comment.Declaration := DeclarationComment;
end;

function RefAttribute(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Content:            TStrictContent
): TNamedAttribute;
begin
  RefAttribute         := RefAttribute(Name, DeclarationComment);
  RefAttribute.Content := Content;
end;

function RefAttribute(
  const Name:              TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment: TStrictDefinitionComment
): TNamedAttribute;
begin
  RefAttribute                    := RefAttribute(Name, DeclarationComment);
  RefAttribute.Comment.Definition := DefinitionComment;
end;

function RefAttribute(
  const Name:              TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment: TStrictDefinitionComment;
  const Content:            TStrictContent
): TNamedAttribute;
begin
  RefAttribute         := RefAttribute(Name, DeclarationComment, DefinitionComment);
  RefAttribute.Content := Content;
end;

function Attributes(const OpenArray: array of TNamedAttribute): NamedAttributes;
var Index: Integer;
begin
  SetLength(Attributes, Length(OpenArray));
  for Index := 0 to High(OpenArray) do
    Attributes[Index] := OpenArray[Index];
end;


function Node(const Name: TStrictName): TNamedNode;
begin
  Node.IsRef := False;
  Node.Name  := Name;
end;

function Node(
  const Name:    TStrictName;
  const Comment: TStrictComment
): TNamedNode;
begin
  Node := Node(Name);
  Node.Comment.Declaration.Value := Comment.Value;
  Node.Comment.Delimeter         := Comment.Delimeter;
end;

function Node(
  const Name: TStrictName;
  const Children: TNamedNodes
): TNamedNode;
begin
  Node          := Node(Name);
  Node.Children := Children;
end;

function Node(
  const Name: TStrictName;
  const Attributes: NamedAttributes
): TNamedNode;
begin
  Node            := Node(Name);
  Node.Attributes := Attributes;
end;

function Node(
  const Name: TStrictName;
  const Children: TNamedNodes;
  const Attributes: NamedAttributes
): TNamedNode;
begin
  Node            := Node(Name, Children);
  Node.Attributes := Attributes;
end;

function Node(
  const Name:     TStrictName;
  const Comment:  TStrictComment;
  const Children: TNamedNodes
): TNamedNode;
begin
  Node          := Node(Name, Comment);
  Node.Children := Children;
end;

function Node(
  const Name:     TStrictName;
  const Comment:  TStrictComment;
  const Attributes: NamedAttributes
): TNamedNode;
begin
  Node            := Node(Name, Comment);
  Node.Attributes := Attributes;
end;

function Node(
  const Name:       TStrictName;
  const Comment:    TStrictComment;
  const Children:   TNamedNodes;
  const Attributes: NamedAttributes
): TNamedNode;
begin
  Node            := Node(Name, Comment, Children);
  Node.Attributes := Attributes;
end;

function RefNode(const Name: TStrictName): TNamedNode;
begin
  RefNode.IsRef := True;
  RefNode.Name  := Name;
end;

function RefNode(
  const Name:     TStrictName;
  const Children: TNamedNodes
  ): TNamedNode;
begin
  RefNode          := RefNode(Name);
  RefNode.Children := Children;
end;

function RefNode(
  const Name:       TStrictName;
  const Attributes: NamedAttributes
  ): TNamedNode;
begin
  RefNode            := RefNode(Name);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:       TStrictName;
  const Children:   TNamedNodes;
  const Attributes: NamedAttributes
  ): TNamedNode;
begin
  RefNode            := RefNode(Name, Children);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment
): TNamedNode;
begin
  RefNode                     := RefNode(Name);
  RefNode.Comment.Declaration := DeclarationComment;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Children:           TNamedNodes
  ): TNamedNode;
begin
  RefNode := RefNode(Name, DeclarationComment);
  RefNode.Children := Children;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Attributes:         NamedAttributes
  ): TNamedNode;
begin
  RefNode            := RefNode(Name, DeclarationComment);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const Children:           TNamedNodes;
  const Attributes:         NamedAttributes
  ): TNamedNode;
begin
  RefNode            := RefNode(Name, DeclarationComment, Children);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment
): TNamedNode;
begin
  RefNode                    := RefNode(Name, DeclarationComment);
  RefNode.Comment.Definition := DefinitionComment;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment;
  const Children: TNamedNodes
): TNamedNode;
begin
  RefNode          := RefNode(Name, DeclarationComment, DefinitionComment);
  RefNode.Children := Children;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment;
  const Attributes:         NamedAttributes
): TNamedNode;
begin
  RefNode            := RefNode(Name, DeclarationComment, DefinitionComment);
  RefNode.Attributes := Attributes;
end;

function RefNode(
  const Name:               TStrictName;
  const DeclarationComment: TStrictDeclarationComment;
  const DefinitionComment:  TStrictDefinitionComment;
  const Children:           TNamedNodes;
  const Attributes:         NamedAttributes
): TNamedNode;
begin
  RefNode := RefNode(Name, DeclarationComment, DefinitionComment, Children);
  RefNode.Attributes := Attributes;
end;

function Nodes(const OpenArray: array of TNamedNode): TNamedNodes;
var Index: Integer;
begin
  SetLength(Nodes, Length(OpenArray));
  for Index := 0 to High(OpenArray) do
    Nodes[Index] := OpenArray[Index];
end;

function TreeStructInfo(const Name: TStrictName): TTSInfoTree;
begin
  TreeStructInfo := TTSInfoTree.Create;
  TreeStructInfo.RenameTree(Name.Value);
end;

function TreeStructInfo(
  const Name:  TStrictName;
  const Nodes: TNamedNodes
): TTSINfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name);
  TSDSLHelpers.AddNodesToTree(TreeStructInfo, '', Nodes);
end;

function TreeStructInfo(
  const Name:       TStrictName;
  const Attributes: NamedAttributes
): TTSINfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name);
  TSDSLHelpers.AddAttributesToTree(TreeStructInfo, '', Attributes);
end;

function TreeStructInfo(
  const Name:       TStrictName;
  const Nodes:      TNamedNodes;
  const Attributes: NamedAttributes
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name, Nodes);
  TSDSLHelpers.AddAttributesToTree(TreeStructInfo, '', Attributes);
end;

function TreeStructInfo(
  const Name:    TStrictName;
  const Comment: TStrictComment
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name);
  TreeStructInfo.WriteTreeComment(Comment.Value, '');
end;

function TreeStructInfo(
  const Name:    TStrictName;
  const Comment: TStrictComment;
  const Nodes:   TNamedNodes
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name, Comment);
  TSDSLHelpers.AddNodesToTree(TreeStructInfo, '', Nodes);
end;

function TreeStructInfo(
  const Name:       TStrictName;
  const Comment:    TStrictComment;
  const Nodes:      TNamedNodes;
  const Attributes: NamedAttributes
): TTSInfoTree;
begin
  TreeStructInfo := TreeStructInfo(Name, Comment, Nodes);
  TSDSLHelpers.AddAttributesToTree(TreeStructInfo, '', Attributes);
end;

end.

