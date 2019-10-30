/* @generated from adl module sys.adlast */

package org.adl.runtime.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Objects;

public class Union {

  /* Members */

  private ArrayList<String> typeParams;
  private ArrayList<Field> fields;

  /* Constructors */

  public Union(ArrayList<String> typeParams, ArrayList<Field> fields) {
    this.typeParams = Objects.requireNonNull(typeParams);
    this.fields = Objects.requireNonNull(fields);
  }

  public Union() {
    this.typeParams = new ArrayList<String>();
    this.fields = new ArrayList<Field>();
  }

  public Union(Union other) {
    this.typeParams = Factories.arrayList(Factories.STRING).create(other.typeParams);
    this.fields = Factories.arrayList(Field.FACTORY).create(other.fields);
  }

  /* Accessors and mutators */

  public ArrayList<String> getTypeParams() {
    return typeParams;
  }

  public void setTypeParams(ArrayList<String> typeParams) {
    this.typeParams = Objects.requireNonNull(typeParams);
  }

  public ArrayList<Field> getFields() {
    return fields;
  }

  public void setFields(ArrayList<Field> fields) {
    this.fields = Objects.requireNonNull(fields);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Union)) {
      return false;
    }
    Union other = (Union) other0;
    return
      typeParams.equals(other.typeParams) &&
      fields.equals(other.fields);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + typeParams.hashCode();
    _result = _result * 37 + fields.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private ArrayList<String> typeParams;
    private ArrayList<Field> fields;

    public Builder() {
      this.typeParams = null;
      this.fields = null;
    }

    public Builder setTypeParams(ArrayList<String> typeParams) {
      this.typeParams = Objects.requireNonNull(typeParams);
      return this;
    }

    public Builder setFields(ArrayList<Field> fields) {
      this.fields = Objects.requireNonNull(fields);
      return this;
    }

    public Union create() {
      Builders.checkFieldInitialized("Union", "typeParams", typeParams);
      Builders.checkFieldInitialized("Union", "fields", fields);
      return new Union(typeParams, fields);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<Union> FACTORY = new Factory<Union>() {
    @Override
    public Union create() {
      return new Union();
    }

    @Override
    public Union create(Union other) {
      return new Union(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("sys.adlast", "Union");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Union> jsonBinding() {
      return Union.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Union> jsonBinding() {
    final Lazy<JsonBinding<ArrayList<String>>> typeParams = new Lazy<>(() -> JsonBindings.arrayList(JsonBindings.STRING));
    final Lazy<JsonBinding<ArrayList<Field>>> fields = new Lazy<>(() -> JsonBindings.arrayList(Field.jsonBinding()));
    final Factory<Union> _factory = FACTORY;

    return new JsonBinding<Union>() {
      @Override
      public Factory<Union> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Union _value) {
        JsonObject _result = new JsonObject();
        _result.add("typeParams", typeParams.get().toJson(_value.typeParams));
        _result.add("fields", fields.get().toJson(_value.fields));
        return _result;
      }

      @Override
      public Union fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Union(
          JsonBindings.fieldFromJson(_obj, "typeParams", typeParams.get()),
          JsonBindings.fieldFromJson(_obj, "fields", fields.get())
        );
      }
    };
  }
}
