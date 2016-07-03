unit TestNamedAttribute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TSDSL, TSDSLTypes;

type

  NamedAttributeTestCase= class(TTestCase)
  private
    Attr: TNamedAttribute;
  published
    procedure AttributeWithName;
    procedure AttributeWithNameAndComment;
    procedure AttributeWithNameAndContent;
    procedure AttributeWithNameCommentAndContent;

    procedure RefAttributeWithName;
    procedure RefAttributeWithNameAndContent;
    procedure RefAttributeWithNameAndDeclComment;
    procedure RefAttributeWithNameDeclCommentAndContent;
    procedure RefAttributeWithNameDeclCommentAndDefComment;
    procedure RefAttributeWithNameDeclCommentDefCommentAndContent;
  end;

implementation

procedure NamedAttributeTestCase.AttributeWithName;
begin
  Attr := Attribute(
    Name('attr name')
  );

  AssertEquals(Attr.Name.Value, 'attr name');
end;

procedure NamedAttributeTestCase.AttributeWithNameAndComment;
begin
  Attr := Attribute(
    Name('attr name'),
    Comment('attr comment')
  );

  AssertEquals(Attr.Name.Value,                'attr name');
  AssertEquals(Attr.Comment.Declaration.Value, 'attr comment');
end;

procedure NamedAttributeTestCase.AttributeWithNameAndContent;
begin
  Attr := Attribute(
    Name('name'),
    Content('content')
  );

  AssertEquals(Attr.Name.Value,    'name');
  AssertEquals(Attr.Content.Value, 'content');
end;

procedure NamedAttributeTestCase.AttributeWithNameCommentAndContent;
begin
  Attr := Attribute(
    Name('attr name'),
    Comment('attr comment'),
    Content('attr content')
  );

  AssertEquals(Attr.Name.Value,                'attr name');
  AssertEquals(Attr.Comment.Declaration.Value, 'attr comment');
  AssertEquals(Attr.Content.Value,             'attr content');
end;

procedure NamedAttributeTestCase.RefAttributeWithName;
begin
  Attr := RefAttribute(
    Name('name')
  );

  AssertEquals(Attr.Name.Value, 'name');
end;

procedure NamedAttributeTestCase.RefAttributeWithNameAndContent;
begin
  Attr := RefAttribute(
    Name('name'),
    Content('content')
  );

  AssertEquals(Attr.Name.Value,    'name');
  AssertEquals(Attr.Content.Value, 'content');
end;

procedure NamedAttributeTestCase.RefAttributeWithNameAndDeclComment;
begin
  Attr := RefAttribute(
    Name('name'),
    DeclarationComment('decl comment')
  );

  AssertEquals(Attr.Name.Value,                'name');
  AssertEquals(Attr.Comment.Declaration.Value, 'decl comment');
end;

procedure NamedAttributeTestCase.RefAttributeWithNameDeclCommentAndContent;
begin
  Attr := RefAttribute(
    Name('name'),
    DeclarationComment('decl comment'),
    Content('content')
  );

  AssertEquals(Attr.Name.Value,                'name');
  AssertEquals(Attr.Comment.Declaration.Value, 'decl comment');
  AssertEquals(Attr.Content.Value,             'content');
end;

procedure NamedAttributeTestCase.RefAttributeWithNameDeclCommentAndDefComment;
begin
  Attr := RefAttribute(
    Name('name'),
    DeclarationComment('decl comment'),
    DefinitionComment('def comment')
  );

  AssertEquals(Attr.Name.Value,                'name');
  AssertEquals(Attr.Comment.Declaration.Value, 'decl comment');
  AssertEquals(Attr.Comment.Definition.Value,  'def comment');
end;

procedure NamedAttributeTestCase.RefAttributeWithNameDeclCommentDefCommentAndContent;
begin
  Attr := RefAttribute(
    Name('name'),
    DeclarationComment('decl comment'),
    DefinitionComment('def comment'),
    Content('content')
  );

  AssertEquals(Attr.Name.Value,                'name');
  AssertEquals(Attr.Comment.Declaration.Value, 'decl comment');
  AssertEquals(Attr.Comment.Definition.Value,  'def comment');
  AssertEquals(Attr.Content.Value,             'content');
end;

initialization

  RegisterTest(NamedAttributeTestCase);
end.

