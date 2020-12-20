/* @generated from adl module test4 */

package org.adl.test4;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import helpers.CDateHelpers;
import helpers.DateHelpers;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.HashMapHelpers;
import org.adl.runtime.HashSetHelpers;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.MaybeHelpers;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import org.adl.runtime.sys.types.MapEntry;
import org.adl.runtime.sys.types.Pair;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public class S {

  /* Members */

  private LocalDate v1;
  private LocalDate v2;
  private LocalDate v3;
  private LocalDate v4;
  private Optional<String> v5;
  private Optional<String> v5a;
  private Optional<String> v5b;
  private Pair<String, Integer> v6;
  private Set<Integer> v7;
  private Set<Integer> v7a;
  private Map<String, Integer> v8;
  private Map<String, Integer> v8a;

  /* Constructors */

  public S(LocalDate v1, LocalDate v2, LocalDate v3, LocalDate v4, Optional<String> v5, Optional<String> v5a, Optional<String> v5b, Pair<String, Integer> v6, Set<Integer> v7, Set<Integer> v7a, Map<String, Integer> v8, Map<String, Integer> v8a) {
    this.v1 = Objects.requireNonNull(v1);
    this.v2 = Objects.requireNonNull(v2);
    this.v3 = Objects.requireNonNull(v3);
    this.v4 = Objects.requireNonNull(v4);
    this.v5 = Objects.requireNonNull(v5);
    this.v5a = Objects.requireNonNull(v5a);
    this.v5b = Objects.requireNonNull(v5b);
    this.v6 = Objects.requireNonNull(v6);
    this.v7 = Objects.requireNonNull(v7);
    this.v7a = Objects.requireNonNull(v7a);
    this.v8 = Objects.requireNonNull(v8);
    this.v8a = Objects.requireNonNull(v8a);
  }

  public S(LocalDate v1, LocalDate v3, Optional<String> v5, Pair<String, Integer> v6, Set<Integer> v7a, Map<String, Integer> v8) {
    this.v1 = Objects.requireNonNull(v1);
    this.v2 = defV2();
    this.v3 = Objects.requireNonNull(v3);
    this.v4 = defV4();
    this.v5 = Objects.requireNonNull(v5);
    this.v5a = defV5a();
    this.v5b = defV5b();
    this.v6 = Objects.requireNonNull(v6);
    this.v7 = defV7();
    this.v7a = Objects.requireNonNull(v7a);
    this.v8 = Objects.requireNonNull(v8);
    this.v8a = defV8a();
  }

  public S(S other) {
    this.v1 = DateHelpers.FACTORY.create(other.v1);
    this.v2 = DateHelpers.FACTORY.create(other.v2);
    this.v3 = CDateHelpers.FACTORY.create(other.v3);
    this.v4 = CDateHelpers.FACTORY.create(other.v4);
    this.v5 = MaybeHelpers.factory(Factories.STRING).create(other.v5);
    this.v5a = MaybeHelpers.factory(Factories.STRING).create(other.v5a);
    this.v5b = MaybeHelpers.factory(Factories.STRING).create(other.v5b);
    this.v6 = Pair.factory(Factories.STRING, Factories.INT32).create(other.v6);
    this.v7 = HashSetHelpers.factory(Factories.INT32).create(other.v7);
    this.v7a = HashSetHelpers.factory(Factories.INT32).create(other.v7a);
    this.v8 = HashMapHelpers.factory(Factories.STRING, Factories.INT32).create(other.v8);
    this.v8a = HashMapHelpers.factory(Factories.STRING, Factories.INT32).create(other.v8a);
  }

  /* Field defaults */

  public static LocalDate defV2() {
    return DateHelpers.create("2000-01-01");
  }

  public static LocalDate defV4() {
    return CDateHelpers.create((short)2000, (short)1, (short)1);
  }

  public static Optional<String> defV5a() {
    return MaybeHelpers.nothing(null);
  }

  public static Optional<String> defV5b() {
    return MaybeHelpers.just("hello");
  }

  public static Set<Integer> defV7() {
    return HashSetHelpers.create(Factories.list(1, 2, 3));
  }

  public static Map<String, Integer> defV8a() {
    return HashMapHelpers.create(Factories.list(new MapEntry<String, Integer>("X", 1), new MapEntry<String, Integer>("Y", 2)));
  }

  /* Accessors and mutators */

  public LocalDate getV1() {
    return v1;
  }

  public S setV1(LocalDate v1) {
    this.v1 = Objects.requireNonNull(v1);
    return this;
  }

  public LocalDate getV2() {
    return v2;
  }

  public S setV2(LocalDate v2) {
    this.v2 = Objects.requireNonNull(v2);
    return this;
  }

  public LocalDate getV3() {
    return v3;
  }

  public S setV3(LocalDate v3) {
    this.v3 = Objects.requireNonNull(v3);
    return this;
  }

  public LocalDate getV4() {
    return v4;
  }

  public S setV4(LocalDate v4) {
    this.v4 = Objects.requireNonNull(v4);
    return this;
  }

  public Optional<String> getV5() {
    return v5;
  }

  public S setV5(Optional<String> v5) {
    this.v5 = Objects.requireNonNull(v5);
    return this;
  }

  public Optional<String> getV5a() {
    return v5a;
  }

  public S setV5a(Optional<String> v5a) {
    this.v5a = Objects.requireNonNull(v5a);
    return this;
  }

  public Optional<String> getV5b() {
    return v5b;
  }

  public S setV5b(Optional<String> v5b) {
    this.v5b = Objects.requireNonNull(v5b);
    return this;
  }

  public Pair<String, Integer> getV6() {
    return v6;
  }

  public S setV6(Pair<String, Integer> v6) {
    this.v6 = Objects.requireNonNull(v6);
    return this;
  }

  public Set<Integer> getV7() {
    return v7;
  }

  public S setV7(Set<Integer> v7) {
    this.v7 = Objects.requireNonNull(v7);
    return this;
  }

  public Set<Integer> getV7a() {
    return v7a;
  }

  public S setV7a(Set<Integer> v7a) {
    this.v7a = Objects.requireNonNull(v7a);
    return this;
  }

  public Map<String, Integer> getV8() {
    return v8;
  }

  public S setV8(Map<String, Integer> v8) {
    this.v8 = Objects.requireNonNull(v8);
    return this;
  }

  public Map<String, Integer> getV8a() {
    return v8a;
  }

  public S setV8a(Map<String, Integer> v8a) {
    this.v8a = Objects.requireNonNull(v8a);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S other = (S) other0;
    return
      v1.equals(other.v1) &&
      v2.equals(other.v2) &&
      v3.equals(other.v3) &&
      v4.equals(other.v4) &&
      v5.equals(other.v5) &&
      v5a.equals(other.v5a) &&
      v5b.equals(other.v5b) &&
      v6.equals(other.v6) &&
      v7.equals(other.v7) &&
      v7a.equals(other.v7a) &&
      v8.equals(other.v8) &&
      v8a.equals(other.v8a);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + v1.hashCode();
    _result = _result * 37 + v2.hashCode();
    _result = _result * 37 + v3.hashCode();
    _result = _result * 37 + v4.hashCode();
    _result = _result * 37 + v5.hashCode();
    _result = _result * 37 + v5a.hashCode();
    _result = _result * 37 + v5b.hashCode();
    _result = _result * 37 + v6.hashCode();
    _result = _result * 37 + v7.hashCode();
    _result = _result * 37 + v7a.hashCode();
    _result = _result * 37 + v8.hashCode();
    _result = _result * 37 + v8a.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private LocalDate v1;
    private LocalDate v2;
    private LocalDate v3;
    private LocalDate v4;
    private Optional<String> v5;
    private Optional<String> v5a;
    private Optional<String> v5b;
    private Pair<String, Integer> v6;
    private Set<Integer> v7;
    private Set<Integer> v7a;
    private Map<String, Integer> v8;
    private Map<String, Integer> v8a;

    public Builder() {
      this.v1 = null;
      this.v2 = DateHelpers.create("2000-01-01");
      this.v3 = null;
      this.v4 = CDateHelpers.create((short)2000, (short)1, (short)1);
      this.v5 = null;
      this.v5a = MaybeHelpers.nothing(null);
      this.v5b = MaybeHelpers.just("hello");
      this.v6 = null;
      this.v7 = HashSetHelpers.create(Factories.list(1, 2, 3));
      this.v7a = null;
      this.v8 = null;
      this.v8a = HashMapHelpers.create(Factories.list(new MapEntry<String, Integer>("X", 1), new MapEntry<String, Integer>("Y", 2)));
    }

    public Builder setV1(LocalDate v1) {
      this.v1 = Objects.requireNonNull(v1);
      return this;
    }

    public Builder setV2(LocalDate v2) {
      this.v2 = Objects.requireNonNull(v2);
      return this;
    }

    public Builder setV3(LocalDate v3) {
      this.v3 = Objects.requireNonNull(v3);
      return this;
    }

    public Builder setV4(LocalDate v4) {
      this.v4 = Objects.requireNonNull(v4);
      return this;
    }

    public Builder setV5(Optional<String> v5) {
      this.v5 = Objects.requireNonNull(v5);
      return this;
    }

    public Builder setV5a(Optional<String> v5a) {
      this.v5a = Objects.requireNonNull(v5a);
      return this;
    }

    public Builder setV5b(Optional<String> v5b) {
      this.v5b = Objects.requireNonNull(v5b);
      return this;
    }

    public Builder setV6(Pair<String, Integer> v6) {
      this.v6 = Objects.requireNonNull(v6);
      return this;
    }

    public Builder setV7(Set<Integer> v7) {
      this.v7 = Objects.requireNonNull(v7);
      return this;
    }

    public Builder setV7a(Set<Integer> v7a) {
      this.v7a = Objects.requireNonNull(v7a);
      return this;
    }

    public Builder setV8(Map<String, Integer> v8) {
      this.v8 = Objects.requireNonNull(v8);
      return this;
    }

    public Builder setV8a(Map<String, Integer> v8a) {
      this.v8a = Objects.requireNonNull(v8a);
      return this;
    }

    public S create() {
      Builders.checkFieldInitialized("S", "v1", v1);
      Builders.checkFieldInitialized("S", "v3", v3);
      Builders.checkFieldInitialized("S", "v5", v5);
      Builders.checkFieldInitialized("S", "v6", v6);
      Builders.checkFieldInitialized("S", "v7a", v7a);
      Builders.checkFieldInitialized("S", "v8", v8);
      return new S(v1, v2, v3, v4, v5, v5a, v5b, v6, v7, v7a, v8, v8a);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S> FACTORY = new Factory<S>() {
    @Override
    public S create(S other) {
      return new S(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test4", "S");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S> jsonBinding() {
      return S.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S> jsonBinding() {
    final Lazy<JsonBinding<LocalDate>> v1 = new Lazy<>(() -> DateHelpers.jsonBinding());
    final Lazy<JsonBinding<LocalDate>> v2 = new Lazy<>(() -> DateHelpers.jsonBinding());
    final Lazy<JsonBinding<LocalDate>> v3 = new Lazy<>(() -> CDateHelpers.jsonBinding());
    final Lazy<JsonBinding<LocalDate>> v4 = new Lazy<>(() -> CDateHelpers.jsonBinding());
    final Lazy<JsonBinding<Optional<String>>> v5 = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<Optional<String>>> v5a = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<Optional<String>>> v5b = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<Pair<String, Integer>>> v6 = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.STRING, JsonBindings.INT32));
    final Lazy<JsonBinding<Set<Integer>>> v7 = new Lazy<>(() -> HashSetHelpers.jsonBinding(JsonBindings.INT32));
    final Lazy<JsonBinding<Set<Integer>>> v7a = new Lazy<>(() -> HashSetHelpers.jsonBinding(JsonBindings.INT32));
    final Lazy<JsonBinding<Map<String, Integer>>> v8 = new Lazy<>(() -> HashMapHelpers.jsonBinding(JsonBindings.STRING, JsonBindings.INT32));
    final Lazy<JsonBinding<Map<String, Integer>>> v8a = new Lazy<>(() -> HashMapHelpers.jsonBinding(JsonBindings.STRING, JsonBindings.INT32));
    final Factory<S> _factory = FACTORY;

    return new JsonBinding<S>() {
      @Override
      public Factory<S> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S _value) {
        JsonObject _result = new JsonObject();
        _result.add("v1", v1.get().toJson(_value.v1));
        _result.add("v2", v2.get().toJson(_value.v2));
        _result.add("v3", v3.get().toJson(_value.v3));
        _result.add("v4", v4.get().toJson(_value.v4));
        _result.add("v5", v5.get().toJson(_value.v5));
        _result.add("v5a", v5a.get().toJson(_value.v5a));
        _result.add("v5b", v5b.get().toJson(_value.v5b));
        _result.add("v6", v6.get().toJson(_value.v6));
        _result.add("v7", v7.get().toJson(_value.v7));
        _result.add("v7a", v7a.get().toJson(_value.v7a));
        _result.add("v8", v8.get().toJson(_value.v8));
        _result.add("v8a", v8a.get().toJson(_value.v8a));
        return _result;
      }

      @Override
      public S fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S(
          JsonBindings.fieldFromJson(_obj, "v1", v1.get()),
          _obj.has("v2") ? JsonBindings.fieldFromJson(_obj, "v2", v2.get()) : DateHelpers.create("2000-01-01"),
          JsonBindings.fieldFromJson(_obj, "v3", v3.get()),
          _obj.has("v4") ? JsonBindings.fieldFromJson(_obj, "v4", v4.get()) : CDateHelpers.create((short)2000, (short)1, (short)1),
          JsonBindings.fieldFromJson(_obj, "v5", v5.get()),
          _obj.has("v5a") ? JsonBindings.fieldFromJson(_obj, "v5a", v5a.get()) : MaybeHelpers.nothing(null),
          _obj.has("v5b") ? JsonBindings.fieldFromJson(_obj, "v5b", v5b.get()) : MaybeHelpers.just("hello"),
          JsonBindings.fieldFromJson(_obj, "v6", v6.get()),
          _obj.has("v7") ? JsonBindings.fieldFromJson(_obj, "v7", v7.get()) : HashSetHelpers.create(Factories.list(1, 2, 3)),
          JsonBindings.fieldFromJson(_obj, "v7a", v7a.get()),
          JsonBindings.fieldFromJson(_obj, "v8", v8.get()),
          _obj.has("v8a") ? JsonBindings.fieldFromJson(_obj, "v8a", v8a.get()) : HashMapHelpers.create(Factories.list(new MapEntry<String, Integer>("X", 1), new MapEntry<String, Integer>("Y", 2)))
        );
      }
    };
  }
}
