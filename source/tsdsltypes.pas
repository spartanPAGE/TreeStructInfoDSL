unit tsdsltypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type TStrictName = record
  Value: String;
end;

type TStrictComment = record
  Value: String;
  Delimeter: String;
end;

type TStrictDeclarationComment = record
  Value: String;
  Delimeter: String;
end;

type TStrictDefinitionComment = record
  Value: String;
  Delimeter: String;
end;

type TStrictSplittedComment = record
  Declaration: TStrictDeclarationComment;
  Definition: TStrictDefinitionComment;
  Delimeter: String;
end;

type TStrictContent = record
  Value: String;
end;

type TNamedAttribute = record
  Name:    TStrictName;
  Comment: TStrictSplittedComment;
  Content: TStrictContent;
  IsRef:   Boolean;
end;
type TNamedAttributes = array of TNamedAttribute;


type TNamedNode = record
  Name:       TStrictName;
  Comment:    TStrictSplittedComment;
  Children:   array of TNamedNode;
  Attributes: TNamedAttributes;
  IsRef:      Boolean;
end;
type TNamedNodes = array of TNamedNode;

type TElementUnionType = (elUnionAttribute, elUnionNode);
type TElementUnion = record
  Attribute:  TNamedAttribute;
  Node:       TNamedNode;
  StoredType: TElementUnionType;
end;

type TElementUnions = array of TElementUnion;

operator:=(const Attribute: TNamedAttribute): TElementUnion;

operator:=(const Node: TNamedNode): TElementUnion;

type TSievedElements = record
  Nodes: TNamedNodes;
  Attributes: TNamedAttributes;
end;

function SieveElements(const Elements: array of TElementUnion): TSievedElements;

implementation

function CountElementsOfType(
  const Elements: array of TElementUnion;
  const ElType:   TElementUnionType
): Integer;
var Element: TElementUnion;
begin
  Result := 0;
  for Element in Elements do
  begin
    if Element.StoredType = ElType then
      Inc(Result);
  end;
end;

function FetchNodes(const Elements: array of TElementUnion): TNamedNodes;
var Element: TElementUnion;
var Index:   Integer = 0;
begin
  SetLength(Result, CountElementsOfType(Elements, elUnionNode));
  for Element in Elements do
  begin
    if Element.StoredType = elUnionNode then
    begin
      Result[Index] := Element.Node;
      Inc(Index);
    end;
  end;
end;

function FetchAttributes(const Elements: array of TElementUnion): TNamedAttributes;
var Element: TElementUnion;
var Index:   Integer = 0;
begin
  SetLength(Result, CountElementsOfType(Elements, elUnionAttribute));
  for Element in Elements do
  begin
    if Element.StoredType = elUnionAttribute then
    begin
      Result[Index] := Element.Attribute;
      Inc(Index);
    end;
  end;
end;

function SieveElements(const Elements: array of TElementUnion): TSievedElements;
begin
  Result.Nodes      := FetchNodes(Elements);
  Result.Attributes := FetchAttributes(Elements);
end;

operator:=(const Attribute: TNamedAttribute): TElementUnion;
begin
  Result.Attribute  := Attribute;
  Result.StoredType := elUnionAttribute;
end;

operator:=(const Node: TNamedNode): TElementUnion;
begin
  Result.Node       := Node;
  Result.StoredType := elUnionNode;
end;

end.

