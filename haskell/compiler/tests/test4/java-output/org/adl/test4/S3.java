/* @generated from adl module test4 */

package org.adl.test4;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class S3 {

  /* Members */

  private int intv1;
  private int intv2;

  /* Constructors */

  public S3(int intv1, int intv2) {
    this.intv1 = intv1;
    this.intv2 = intv2;
  }

  public S3(S3 other) {
    this.intv1 = other.intv1;
    this.intv2 = other.intv2;
  }

  /* legacy default ctor */
  public S3() {
    this.intv1 = 0;
    this.intv2 = 0;
  }

  /* Accessors and mutators */

  public int getIntv1() {
    return intv1;
  }

  public S3 setIntv1(int intv1) {
    this.intv1 = intv1;
    return this;
  }

  public int getIntv2() {
    return intv2;
  }

  public S3 setIntv2(int intv2) {
    this.intv2 = intv2;
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S3)) {
      return false;
    }
    S3 other = (S3) other0;
    return
      intv1 == other.intv1 &&
      intv2 == other.intv2;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + intv1;
    _result = _result * 37 + intv2;
    return _result;
  }

  /* Builder */

  public static class Builder {
    private Integer intv1;
    private Integer intv2;

    public Builder() {
      this.intv1 = null;
      this.intv2 = null;
    }

    public Builder setIntv1(Integer intv1) {
      this.intv1 = Objects.requireNonNull(intv1);
      return this;
    }

    public Builder setIntv2(Integer intv2) {
      this.intv2 = Objects.requireNonNull(intv2);
      return this;
    }

    public S3 create() {
      Builders.checkFieldInitialized("S3", "intv1", intv1);
      Builders.checkFieldInitialized("S3", "intv2", intv2);
      return new S3(intv1, intv2);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S3> FACTORY = new Factory<S3>() {
    @Override
    public S3 create(S3 other) {
      return new S3(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test4", "S3");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S3> jsonBinding() {
      return S3.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S3> jsonBinding() {
    final Lazy<JsonBinding<Integer>> intv1 = new Lazy<>(() -> JsonBindings.INT32);
    final Lazy<JsonBinding<Integer>> intv2 = new Lazy<>(() -> JsonBindings.INT32);
    final Factory<S3> _factory = FACTORY;

    return new JsonBinding<S3>() {
      @Override
      public Factory<S3> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S3 _value) {
        JsonObject _result = new JsonObject();
        _result.add("intv1", intv1.get().toJson(_value.intv1));
        _result.add("intv2", intv2.get().toJson(_value.intv2));
        return _result;
      }

      @Override
      public S3 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S3(
          JsonBindings.fieldFromJson(_obj, "intv1", intv1.get()),
          JsonBindings.fieldFromJson(_obj, "intv2", intv2.get())
        );
      }
    };
  }
}
