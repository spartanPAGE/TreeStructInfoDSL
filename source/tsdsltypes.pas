unit tsdsltypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type StrictName = record
  Value: String;
end;

type StrictComment = record
  Value: String;
  Delimeter: String;
end;

type StrictContent = record
  Value: String;
end;


type NamedAttribute = record
  Name:    StrictName;
  Comment: StrictComment;
  Content: StrictContent;
end;
type NamedAttributes = array of NamedAttribute;


type NamedNode = record
  Name:       StrictName;
  Comment:    StrictComment;
  Children:   array of NamedNode;
  Attributes: NamedAttributes;
end;
type NamedNodes = array of NamedNode;


implementation

end.

