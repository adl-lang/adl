/* Code generated from adl module sys.adlast */

package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
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
    int result = 1;
    result = result * 37 + typeParams.hashCode();
    result = result * 37 + fields.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Union> FACTORY = new Factory<Union>() {
    public Union create() {
      return new Union();
    }
    public Union create(Union other) {
      return new Union(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Union> jsonBinding() {
    final Lazy<JsonBinding<ArrayList<String>>> typeParams = new Lazy<>(() -> JsonBindings.arrayList(JsonBindings.STRING));
    final Lazy<JsonBinding<ArrayList<Field>>> fields = new Lazy<>(() -> JsonBindings.arrayList(Field.jsonBinding()));
    final Factory<Union> _factory = FACTORY;

    return new JsonBinding<Union>() {
      public Factory<Union> factory() {
        return _factory;
      }

      public JsonElement toJson(Union _value) {
        JsonObject _result = new JsonObject();
        _result.add("typeParams", typeParams.get().toJson(_value.typeParams));
        _result.add("fields", fields.get().toJson(_value.fields));
        return _result;
      }

      public Union fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Union(
          _obj.has("typeParams") ? typeParams.get().fromJson(_obj.get("typeParams")) : new ArrayList<String>(),
          _obj.has("fields") ? fields.get().fromJson(_obj.get("fields")) : new ArrayList<Field>()
        );
      }
    };
  }
}
