import {JsonBinding,createJsonBinding} from './build/runtime/json.ts'
import {fromDynamic, toDynamic} from './build/runtime/dynamic.ts'
import * as example from './build/example.ts'
import {Dynamic} from './build/sys/dynamic.ts'
import {RESOLVER} from './build/resolver.ts'

import { assert, assertEquals } from "https://deno.land/std@0.85.0/testing/asserts.ts";

const personJsonBinding : JsonBinding<example.Person> = createJsonBinding(RESOLVER,example.texprPerson());

const person1 : example.Person = {
  name : "Joe",
  age : 142,
  gender : 'male',
  married : true
};

Deno.test('Person - roundtrips', () => {
  const json = personJsonBinding.toJson(person1);
  const person2 = personJsonBinding.fromJson(json);
  assertEquals(person2.name, person1.name);
  assertEquals(person2.age, person1.age);
  assertEquals(person2.gender, person1.gender);
  assertEquals(person2.married, person1.married);
});

Deno.test( 'Person - constructs with correct defaulting', () => {
  const person = example.makePerson({
    name : 'Sarah',
    gender : 'female',
  });
  assertEquals(person.age, 50);
  assertEquals(person.married, false);
});

Deno.test( 'Person - deserialises with correct defaulting', () => {
  const json = {name:'Tim',gender:'male'};
  const person = personJsonBinding.fromJson(json);
  assertEquals(person.age, 50);
  assertEquals(person.married, false);
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

Deno.test( 'IntTree Recursive Type - roundtrips via json', () => {
  const json = intTreeJsonBinding.toJson(tree1);
  const tree2 = intTreeJsonBinding.fromJson(json);
  assertEquals(tree2.value, 7);
  assertEquals(tree2.children[0].value, 15);
  assertEquals(tree2.children[1].value, 33);
})
Deno.test( 'IntTree Recursive Type - uses custom serialized names', () => {
  const json = intTreeJsonBinding.toJson(tree1) as any;
  assertEquals(json && json["v"], 7);
  assertEquals(json && json["cs"][0].v, 15);
  assertEquals(json && json["cs"][1].v, 33);
})

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

Deno.test('DataSource union - roundtrips', () => {
  const jb = dataSourceJsonBinding;
  const dataSource1r = jb.fromJson(jb.toJson(dataSource1));
  assertEquals(dataSource1r.kind, "implicit");
  const dataSource2r = jb.fromJson(jb.toJson(dataSource2));
  assertEquals(dataSource2r.kind, "inline");
  if (dataSource2r.kind == "inline") {
    assertEquals(dataSource2r.value.encoding, "utf-8");
  }
  const dataSource3r = jb.fromJson(jb.toJson(dataSource3));
  assertEquals(dataSource3r.kind, "file");
  if (dataSource3r.kind == "file") {
    assertEquals(dataSource3r.value, "/tmp/testdata.txt");
  }

  const dataSource4r = jb.fromJson(jb.toJson(dataSource4));
  assertEquals(dataSource4r.kind, "inlinebinary");
  if (dataSource4r.kind == "inlinebinary") {
    assertEquals(dataSource4r.value[0], 100);
    assertEquals(dataSource4r.value[2], 233);
  }
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

Deno.test( 'GenderTree Constructed Concrete Type - roundtrips via json', () => {
  const json = genderTreeJsonBinding.toJson(gtree1);
  const gtree2 = genderTreeJsonBinding.fromJson(json);
  assertEquals(gtree2.value, 'female');
  assertEquals(gtree2.children[0].value, 'male');
  assertEquals(gtree2.children[1].value, 'female');
})

//----------------------------------------------------------------------
const structWithDefaultsJsonBinding : JsonBinding<example.StructWithDefaults>
    = createJsonBinding(RESOLVER,example.texprStructWithDefaults());

Deno.test( 'StructWithDefauilts - correctly inserts default values', () => {
  assertEquals(structWithDefaultsJsonBinding.fromJson({}).field1, null);
  assertEquals(structWithDefaultsJsonBinding.fromJson({}).field2, "hello");
  assertEquals(structWithDefaultsJsonBinding.fromJson({}).field3.name, "Mike");
  assertEquals(structWithDefaultsJsonBinding.fromJson({}).field3.age, 50);
})

//----------------------------------------------------------------------



Deno.test( 'Dynamic Values - can roundtrip values successfully', () => {
  const dynamic : Dynamic = toDynamic(personJsonBinding, person1);
  const person2 : example.Person | null = fromDynamic(personJsonBinding, dynamic);
  assertEquals(person2 && person2.name, "Joe");
  assertEquals(person2 && person2.age, 142);
});
Deno.test( 'Dynamic Values - fails with null when extracting the wrong type', () => {
  const dynamic : Dynamic = toDynamic(personJsonBinding, person1);
  const itree : example.IntTree | null = fromDynamic(intTreeJsonBinding, dynamic);
  assertEquals(itree, null);
});
