import {JsonBinding,createJsonBinding} from './build/runtime/json.ts'
import * as J from './build/runtime/json.ts'
import * as LU from './build/runtime/lifting.ts'
import {fromDynamic, toDynamic} from './build/runtime/dynamic.ts'
import * as example from './build/example.ts'
import * as L from './build/lifting.ts'
import {Dynamic} from './build/sys/dynamic.ts'
import {RESOLVER} from './build/resolver.ts'

import { assert, assertEquals, fail } from "https://deno.land/std@0.85.0/testing/asserts.ts";
import * as T31 from './build/test31.ts';
import { ATypeExpr } from './build/runtime/adl.ts';
import { makeMaybe, texprMaybe } from './build/sys/types.ts';

// Deno.test('TypeDiscrimination01', () => {
//   const td = J.getTypeDiscriminations(RESOLVER, T31.texprMeasure().value)
//   assertEquals(td.max_version, 0)
//   assertEquals(td.type_discs.length, 1)
//   const j1 = JSON.parse(`42`)
//   const j2 = J.liftTypeDiscriminations(RESOLVER, j1, td)
//   assertEquals(j2, JSON.parse(`{"@v":0,"count":42}`))
// });

interface TypeDiscTest {
  name: string
  texpr: ATypeExpr<unknown>
  json: string
  want: unknown
  wantErr?: string
}

Deno.test('TypeDiscrimination01', () => {
  const tests: TypeDiscTest[] = [
    {
      name: "MyStructV2",
      texpr: T31.texprMyStructV2(),
      json: `{"quantity": 42}`,
      want: T31.makeMyStructV2({ quantity: T31.makeMeasure("count", 42) })
    },
    {
      name: "count into measure",
      texpr: T31.texprMeasure(),
      json: `42`,
      want: T31.makeMeasure("count", 42)
    },
    {
      name: "s1 into structtest",
      texpr: T31.texprStructTest(),
      json: `{"quant": 2, "value": 3}`,
      want: T31.makeStructTest("abc", T31.makeS1({ quant: 2, value: 3.0 }))
    },
    {
      name: "void error test",
      texpr: T31.texprVoidTest(),
      json: `null`,
      want: null,
      wantErr: `cannot use Json or Void as a type discriminator`,
    },
    {
      name: "UnionOfLiftedUnion type mismatch error test",
      texpr: T31.texprUnionOfLiftedUnion(),
      json: `null`,
      want: null,
      wantErr: `primitive type mismatch. expected Nullable received {"kind":"reference","value":{"moduleName":"test31","name":"UnionOfLiftedUnion"}}`,
    },
    {
      name: "UnionOfLiftedUnion error test",
      texpr: T31.texprUnionOfLiftedUnion(),
      json: `{"def": {"quant": 2, "value": 3}}`,
      want: null,
      wantErr: `union of union containing TypeDiscrimination branches not supported : ...`,
    },
    {
      name: "UnionUnion a",
      texpr: T31.texprUnionUnion(),
      json: `{"a": "is an a"}`,
      want: T31.makeUnionUnion("abc", T31.makeU1("a", "is an a")),
    },
    {
      name: "UnionUnion b",
      texpr: T31.texprUnionUnion(),
      json: `{"b": 99}`,
      want: T31.makeUnionUnion("abc", T31.makeU1("b", 99)),
    },
    {
      name: "UnionUnion def",
      texpr: T31.texprUnionUnion(),
      json: `{"def": {"quant": 2, "value": 3}}`,
      want: T31.makeUnionUnion("def", T31.makeS1({ quant: 2, value: 3.0 })),
    },
    {
      name: "Struct02Test u1",
      texpr: T31.texprStruct02Test(),
      json: `{"a": "is an a"}`,
      want: T31.makeStruct02Test("u1", T31.makeU1("a", "is an a")),
    },
    {
      name: "Struct02Test u1:b",
      texpr: T31.texprStruct02Test(),
      json: `{"b": 99}`,
      want: T31.makeStruct02Test("u1", T31.makeU1("b", 99)),
    },
    {
      name: "Struct02Test s1",
      texpr: T31.texprStruct02Test(),
      json: `{"quant": 2, "value": 3}`,
      want: T31.makeStruct02Test("s1", T31.makeS1({ quant: 2, value: 3.0 }))
    },
    {
      name: "Struct02Test s1 - no lifting",
      texpr: T31.texprStruct02Test(),
      json: `{"s1":{"quant": 2, "value": 3}}`,
      want: T31.makeStruct02Test("s1", T31.makeS1({ quant: 2, value: 3.0 }))
    },
    {
      name: "NullableTest - a",
      texpr: T31.texprNullableTest(),
      json: `null`,
      want: T31.makeNullableTest("a", null)
    },
    {
      name: "NullableTest - a:sdf",
      texpr: T31.texprNullableTest(),
      json: `"sdf"`,
      want: T31.makeNullableTest("a", "sdf")
    },
    {
      name: "VectorTest - a",
      texpr: T31.texprVectorTest(),
      json: `[]`,
      want: T31.makeVectorTest("a", [])
    },
    {
      name: "VectorErrorTest",
      texpr: T31.texprVectorErrorTest(),
      json: `[]`,
      want: T31.makeVectorErrorTest("a", []),
      wantErr: `ambiguous matching type discriminators a,b`
    },
    {
      name: "VectorOfTypeDiscriminationUnionTest",
      texpr: T31.texprVectorOfTypeDiscriminationUnionTest(),
      json: `{"b":[{"quant": 2, "value": 3},{"a": "is an a"},{"b": 99},{"s1":{"quant": 2, "value": 3}}]}`,
      want: T31.makeVectorOfTypeDiscriminationUnionTest("b", [
        T31.makeStruct02Test("s1", T31.makeS1({ quant: 2, value: 3.0 })),
        T31.makeStruct02Test("u1", T31.makeU1("a", "is an a")),
        T31.makeStruct02Test("u1", T31.makeU1("b", 99)),
        T31.makeStruct02Test("s1", T31.makeS1({ quant: 2, value: 3.0 })),
      ]),
    },
    {
      name: "UnionOfVectorOfTypeDiscriminationUnionTest",
      texpr: T31.texprUnionOfVectorOfTypeDiscriminationUnionTest(),
      json: `[]`,
      want: T31.makeUnionOfVectorOfTypeDiscriminationUnionTest("b", []),
      // wantErr: `union of union containing TypeDiscrimination branches not supported`,
    },
    {
      name: "Ua",
      texpr: T31.texprUa(),
      json: `{"u_b":[{"u_c":[{"a":["def"]}]}]}`,
      want: T31.makeUa("u_b", [
        T31.makeUb("u_c", [
          T31.makeUc("a",["def"])
        ])
      ]),
    },
    {
      name: "Ua - with lifting",
      texpr: T31.texprUa(),
      json: `{"u_b":[{"u_c":[["def"]]}]}`,
      want: T31.makeUa("u_b", [
        T31.makeUb("u_c", [
          T31.makeUc("a",["def"])
        ])
      ]),
    },
    {
      name: "UofMaybe",
      texpr: T31.texprUofMaybe(),
      json: `{"a":{"just":{"abc":{"a":"a string"}}}}`,
      want: T31.makeUofMaybe("a", { kind: "just", value: T31.makeUnionUnion("abc", T31.makeU1("a", "a string")) }),
    },
    {
      name: "UofMaybe - lifting",
      texpr: T31.texprUofMaybe(),
      json: `{"a":{"just":{"a":"a string"}}}`,
      want: T31.makeUofMaybe("a", { kind: "just", value: T31.makeUnionUnion("abc", T31.makeU1("a", "a string")) }),
    },
    {
      name: "UTypeDiscriminationofMaybe - lifting U1",
      texpr: T31.texprUTypeDiscriminationofMaybe(),
      json: `{"a":{"just":{"a":"a string"}}}`,
      want: T31.makeUTypeDiscriminationofMaybe("a", { kind: "just", value: T31.makeUnionUnion("abc", T31.makeU1("a", "a string")) }),
    },
    {
      name: "UTypeDiscriminationofMaybe - lifting U1 & top",
      texpr: T31.texprUTypeDiscriminationofMaybe(),
      json: `{"just":{"a":"a string"}}`,
      want: T31.makeUTypeDiscriminationofMaybe("a", { kind: "just", value: T31.makeUnionUnion("abc", T31.makeU1("a", "a string")) }),
    },
    {
      name: "Maybe - lifting ",
      texpr: texprMaybe( T31.texprU1()),
      json: `{"just":"a string"}`,
      want: makeMaybe("just", T31.makeU1("a", "a string")),
    },
    {
      name: "U1 - lifting ",
      texpr: T31.texprU1(),
      json: `"a string"`,
      want: T31.makeU1("a", "a string"),
    },
    // {
    //   name: "A2",
    //   texpr: T31.texprA2(),
    //   json: `{"a":"a string"}`,
    //   want: T31.makeA2("a", T31.makeA1("a", "a string")),
    // },
    // {
    //   name: "A2 - 2",
    //   texpr: T31.texprA2(),
    //   json: `"a string"`,
    //   want: T31.makeA2("a", T31.makeA1("a", "a string")),
    //   wantErr: "primitive type mismatch. expected String received"
    // },
    {
      name: "Deep",
      texpr: T31.texprA1(),
      json: `"sdaf"`,
      want: T31.makeA1("a", T31.makeA2("b", T31.makeA3("c", T31.makeA4("d", "sdaf"))))
    },
    {
      name: "Deep - lift a4",
      texpr: T31.texprA1(),
      json: `{"d": "sdaf", "@v": 9}`,
      want: T31.makeA1("a", T31.makeA2("b", T31.makeA3("c", T31.makeA4("d", "sdaf"))))
    },
    {
      name: "Deep - lift a3",
      texpr: T31.texprA1(),
      json: `{"c": {"d": "sdaf", "@v": 9}, "@v": 8}`,
      want: T31.makeA1("a", T31.makeA2("b", T31.makeA3("c", T31.makeA4("d", "sdaf"))))
    },
    {
      name: "A3 - lift a3.a4.d",
      texpr: T31.texprA3(),
      json: `{"c": "sdaf", "@v": 8}`,
      want: T31.makeA3("c", T31.makeA4("d", "sdaf")),
    },
    {
      name: "Deep - lift a3.a4.d",
      texpr: T31.texprA1(),
      json: `{"c": "sdaf", "@v": 8}`,
      want: T31.makeA1("a", T31.makeA2("b", T31.makeA3("c", T31.makeA4("d", "sdaf"))))
    },
    {
      name: "UnionUnion - lifting all",
      texpr: T31.texprUnionUnion(),
      json: `"a string"`,
      // want: T31.makeUnionUnion("a", "a string"),
      want: T31.makeUnionUnion("abc", T31.makeU1("a", "a string")),
    },
    {
      name: "UTypeDiscriminationofMaybe - lifting all ",
      texpr: T31.texprUTypeDiscriminationofMaybe(),
      json: `{"just":"a string"}`,
      want: T31.makeUTypeDiscriminationofMaybe("a", { kind: "just", value: T31.makeUnionUnion("abc", T31.makeU1("a", "a string")) }),
    },
    {
      name: "B1 f1",
      texpr: T31.texprB1(),
      json: `"a string"`,
      want: T31.makeB1("f1", "a string"),
    },
    {
      name: "B1 f2",
      texpr: T31.texprB1(),
      json: `["a string"]`,
      want: T31.makeB1("f2", ["a string"]),
    },
    {
      name: "B2 f3",
      texpr: T31.texprB2(),
      json: `["a string"]`,
      want: T31.makeB2("f3", ["a string"]),
    },
    {
      name: "B3 f3",
      texpr: T31.texprB3(),
      json: `["a string"]`,
      want: T31.makeB3("f3", ["a string"]),
    },
    {
      name: "B4 a",
      texpr: T31.texprB4(),
      json: `{"a":"asdfghjkl"}`,
      want: T31.makeB4("b", T31.makeGenU<string,"a">("a", "asdfghjkl")),
    },
    {
      name: "B4 b",
      texpr: T31.texprB4(),
      json: `{"b":{"a":"asdfghjkl"}}`,
      want: T31.makeB4("b", T31.makeGenU<string,"a">("a", "asdfghjkl")),
    },
    {
      name: "B5 a",
      texpr: T31.texprB5(),
      json: `{"a":"asdfghjkl"}`,
      want: T31.makeB5("b", T31.makeGenU<string,"a">("a", "asdfghjkl")),
    },
  ]
  for (const tt of tests) {
    // if (tt.name != "B5 a") {
    //   continue
    // }
    console.log(tt.name)
    const j_in = JSON.parse(tt.json)
    let j_out: J.Json = null
    // console.log(tt.name)
    try {
      const lter = LU.createLifter(RESOLVER, tt.texpr.value)
      j_out = lter.lift(j_in)
    } catch (e) {
      if (tt.wantErr === undefined) {
        console.log(`UNEXPECTED error - '${tt.name}'\n\t${e}`)
        throw e
      }
      console.log(`expected an error - ${tt.name}\n\t${e}`)
      continue
    }
    if (tt.wantErr !== undefined) {
      fail(`EXPECTED AN ERROR : ${tt.name}:\nreceived:${JSON.stringify(j_out)}\n '${tt.wantErr}'`)
    }
    const jb = createJsonBinding(RESOLVER, tt.texpr)
    let o_out: unknown = null
    try {
      o_out = jb.fromJson(j_out)
    } catch (e) {
      const j_wanted = jb.toJson(tt.want)
      console.log(`Exception in ${tt.name}\n  json:    ${JSON.stringify(j_out)}\n  j_wanted:${JSON.stringify(j_wanted)}\n${e.toString()}`, e)
      fail(e)
    }
    assertEquals(o_out, tt.want)
    assertEquals(j_out, jb.toJson(tt.want))
  }

})

//----------------------------------------------------------------------

const liftedJsonBinding: JsonBinding<L.Lifted> = createJsonBinding(RESOLVER, L.texprLifted());
const orgFldJsonBinding: JsonBinding<L.OrgField> = createJsonBinding(RESOLVER, L.texprOrgField());
const oldOutterJB: JsonBinding<L.OldOutter> = createJsonBinding(RESOLVER, L.texprOldOutter());
const newOutterJB: JsonBinding<L.NewOutter> = createJsonBinding(RESOLVER, L.texprNewOutter());

const orgFld: L.OrgField = { "a": "abc", "b": 42 };
const lifted1 = L.makeLifted("org_field", orgFld);

Deno.test('Lifted - decode01', () => {
  // this is not necessary since the json of orgFld and its toJson are the same.
  // only here for completeness
  const json = orgFldJsonBinding.toJson(orgFld);
  const lifted = liftedJsonBinding.fromJson(json);
  assertEquals(lifted.kind, lifted1.kind);
});

Deno.test('Lifted - decode02', () => {
  const oldO = L.makeOldOutter({field0: orgFld})
  const newO = L.makeNewOutter({field0: L.makeLifted("org_field", { "a": "abc", "b": 42 })})
  const json = oldOutterJB.toJson(oldO);
  const lifted = newOutterJB.fromJson(json);
  assertEquals(lifted, newO);
});

Deno.test('Lifted - decode03', () => {
  const jb: JsonBinding<L.LiftedVector> = createJsonBinding(RESOLVER, L.texprLiftedVector());
  const json = ["a", "b", "c"]
  const lifted = jb.fromJson(json);
  assertEquals(lifted.kind, "str_arr");
  if ( lifted.kind == "str_arr" ) {
    assertEquals(lifted.value.length, 3);
  }
  const nonlifted = jb.fromJson({"count_strs": {"v1": 42, "v2": ["d", "e", "f"]}});
  assertEquals(nonlifted.kind, "count_strs");
  if ( nonlifted.kind == "count_strs" ) {
    assertEquals(nonlifted.value.v1, 42);
    assertEquals(nonlifted.value.v2, ["d","e","f"]);
  }
});

//----------------------------------------------------------------------

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

Deno.test('DataSource union - roundtrips - dataSource1', () => {
  const jb = dataSourceJsonBinding;
  const dataSource1r = jb.fromJson(jb.toJson(dataSource1));
  assertEquals(dataSource1r.kind, "implicit");
});
Deno.test('DataSource union - roundtrips - dataSource2', () => {
  const jb = dataSourceJsonBinding;
  const dataSource2r = jb.fromJson(jb.toJson(dataSource2));
  assertEquals(dataSource2r.kind, "inline");
  if (dataSource2r.kind == "inline") {
    assertEquals(dataSource2r.value.encoding, "utf-8");
  }
});
Deno.test('DataSource union - roundtrips - dataSource3', () => {
  const jb = dataSourceJsonBinding;
  const dataSource3r = jb.fromJson(jb.toJson(dataSource3));
  assertEquals(dataSource3r.kind, "file");
  if (dataSource3r.kind == "file") {
    assertEquals(dataSource3r.value, "/tmp/testdata.txt");
  }
});
Deno.test('DataSource union - roundtrips - dataSource4', () => {
  const jb = dataSourceJsonBinding;
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
