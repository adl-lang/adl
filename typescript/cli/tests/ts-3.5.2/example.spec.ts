/// <reference path="node_modules/@types/jest/index.d.ts" />
import {JsonBinding,createJsonBinding} from './build/runtime/json'
import {fromDynamic, toDynamic} from './build/runtime/dynamic'
import * as example from './build/example'
import {Dynamic} from './build/sys/dynamic'
import {RESOLVER} from "./build/resolver";

const personJsonBinding : JsonBinding<example.Person> = createJsonBinding(RESOLVER,example.texprPerson());

const person1 : example.Person = {
  name : "Joe",
  age : 142,
  gender : 'male',
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
      gender : 'female',
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

//----------------------------------------------------------------------

const intTreeJsonBinding : JsonBinding<example.IntTree> = createJsonBinding(RESOLVER,example.texprIntTree());

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
  it( 'uses custom serialized names', () => {
    const json = intTreeJsonBinding.toJson(tree1);
    expect(json && json["v"]).toEqual(7);
    expect(json && json["cs"][0].v).toEqual(15);
    expect(json && json["cs"][1].v).toEqual(33);
  })
});

//----------------------------------------------------------------------

const dataSourceJsonBinding : JsonBinding<example.DataSource>
  = createJsonBinding(RESOLVER,example.texprDataSource());

const dataSource1 : example.DataSource = {
  kind : "implicit"
};

const dataSource2 : example.DataSource = {
  kind : "inline",
  value : {
    encoding : "utf-8",
    content : "Now is the time, or so they said"
  }
};

const dataSource3 : example.DataSource = {
  kind : "file",
  value : "/tmp/testdata.txt"
};

const dataSource4 : example.DataSource = {
  kind : "inlinebinary",
  value : new Uint8Array([100,56,233])
};

describe('DataSource union', () => {
  it( 'roundtrips via json', () => {
    const jb = dataSourceJsonBinding;
    const dataSource1r = jb.fromJson(jb.toJson(dataSource1));
    expect(dataSource1r.kind).toEqual("implicit");
    const dataSource2r = jb.fromJson(jb.toJson(dataSource2));
    expect(dataSource2r.kind).toEqual("inline");
    if (dataSource2r.kind == "inline") {
      expect(dataSource2r.value.encoding).toEqual("utf-8");
    }
    const dataSource3r = jb.fromJson(jb.toJson(dataSource3));
    expect(dataSource3r.kind).toEqual("file");
    if (dataSource3r.kind == "file") {
      expect(dataSource3r.value).toEqual("/tmp/testdata.txt");
    }

    const dataSource4r = jb.fromJson(jb.toJson(dataSource4));
    expect(dataSource4r.kind).toEqual("inlinebinary");
    if (dataSource4r.kind == "inlinebinary") {
      expect(dataSource4r.value[0]).toEqual(100);
      expect(dataSource4r.value[2]).toEqual(233);
    }
  });
});

//----------------------------------------------------------------------

const genderTreeJsonBinding : JsonBinding<example.Tree<example.Gender>>
    = createJsonBinding(RESOLVER,example.texprTree(example.texprGender()));

const gtree1 : example.Tree<example.Gender> = {
  value : 'female',
  children : [
    {
      value : 'male',
      children : [],
    },
    {
      value : 'female',
      children : [],
    }
  ]
};

describe('GenderTree Constructed Concrete Type', () => {
  it( 'roundtrips via json', () => {
    const json = genderTreeJsonBinding.toJson(gtree1);
    const gtree2 = genderTreeJsonBinding.fromJson(json);
    expect(gtree2.value).toEqual('female');
    expect(gtree2.children[0].value).toEqual('male');
    expect(gtree2.children[1].value).toEqual('female');
  })
});

//----------------------------------------------------------------------
const structWithDefaultsJsonBinding : JsonBinding<example.StructWithDefaults>
    = createJsonBinding(RESOLVER,example.texprStructWithDefaults());

describe('StructWithDefaults', () => {
  it( 'correctly inserts default values', () => {
    expect(structWithDefaultsJsonBinding.fromJson({}).field1).toEqual(null);
    expect(structWithDefaultsJsonBinding.fromJson({}).field2).toEqual("hello");
    expect(structWithDefaultsJsonBinding.fromJson({}).field3.name).toEqual("Mike");
    expect(structWithDefaultsJsonBinding.fromJson({}).field3.age).toEqual(50);
  })
});

//----------------------------------------------------------------------



describe('Dynamic Values', () => {
  it( 'can roundtrip values successfully', () => {
    const dynamic : Dynamic = toDynamic(personJsonBinding, person1);
    const person2 : example.Person | null = fromDynamic(personJsonBinding, dynamic);
    expect(person2 && person2.name).toEqual("Joe");
    expect(person2 && person2.age).toEqual(142);
  });
  it( 'fails with null when extracting the wrong type', () => {
    const dynamic : Dynamic = toDynamic(personJsonBinding, person1);
    const itree : example.IntTree | null = fromDynamic(intTreeJsonBinding, dynamic);
    expect(itree).toBeNull();
  });
});
