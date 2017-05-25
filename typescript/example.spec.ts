import {DeclResolver,declResolver} from './build/runtime/adl'
import {JsonBinding,createJsonBinding} from './build/runtime/json'
import * as example from './build/example'
import * as sys_types from './build/sys/types'

const dresolver : DeclResolver = declResolver(
  example._AST_MAP,
  sys_types._AST_MAP
);

const personJsonBinding : JsonBinding<example.Person> = createJsonBinding(dresolver,example.texprPerson());

const person1 : example.Person = {
  name : "Joe",
  age : 142,
  gender : example.Gender.male,
  married : true
};

describe('Person Structure', () => {
  it( 'roundtrips via json', () => {
    const json = personJsonBinding.toJson(person1);
    const person2 = personJsonBinding.fromJson(json);
    expect(person2.name).toEqual(person1.name);
    expect(person2.age).toEqual(person1.age);
    expect(person2.gender).toEqual(person1.gender);
    expect(person2.married).toEqual(person1.married);
  });

  it( 'constructs with correct defaulting', () => {
    const person = example.makePerson({
      name : 'Sarah',
      gender : example.Gender.female
    });
    expect(person.age).toEqual(50);
    expect(person.married).toEqual(false);
  });

  it( 'deserialises with correct defaulting', () => {
    const json = {name:'Tim',gender:'male'};
    const person = personJsonBinding.fromJson(json);
    expect(person.age).toEqual(50);
    expect(person.married).toEqual(false);
  });
});

const intTreeJsonBinding : JsonBinding<example.IntTree> = createJsonBinding(dresolver,example.texprIntTree());

const tree1 : example.IntTree = {
  value : 7,
  children : [
    {
      value : 15,
      children : [],
    },
    {
      value : 33,
      children : [],
    }
  ]
};

describe('IntTree Recursive Type', () => {
  it( 'roundtrips via json', () => {
    const json = intTreeJsonBinding.toJson(tree1);
    const tree2 = intTreeJsonBinding.fromJson(json);
    expect(tree2.value).toEqual(7);
    expect(tree2.children[0].value).toEqual(15);
    expect(tree2.children[1].value).toEqual(33);
  })
});

const genderTreeJsonBinding : JsonBinding<example.Tree<example.Gender>>
    = createJsonBinding(dresolver,example.texprTree(example.texprGender()));

const gtree1 : example.Tree<example.Gender> = {
  value : example.Gender.female,
  children : [
    {
      value : example.Gender.male,
      children : [],
    },
    {
      value : example.Gender.female,
      children : [],
    }
  ]
};

describe('GenderTree Constructed Concrete Type', () => {
  it( 'roundtrips via json', () => {
    const json = genderTreeJsonBinding.toJson(gtree1);
    const gtree2 = genderTreeJsonBinding.fromJson(json);
    expect(gtree2.value).toEqual(example.Gender.female);
    expect(gtree2.children[0].value).toEqual(example.Gender.male);
    expect(gtree2.children[1].value).toEqual(example.Gender.female);
  })
});
