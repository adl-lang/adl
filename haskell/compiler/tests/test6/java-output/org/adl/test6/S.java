/* @generated from adl module test6 */

package org.adl.test6;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
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
import org.adl.runtime.sys.types.Either;
import org.adl.runtime.sys.types.Error;
import org.adl.runtime.sys.types.Pair;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;

public class S {

  /* Members */

  private Pair<Integer, Double> f_pair;
  private Either<String, Integer> f_either;
  private Error<Integer> f_error;
  private HashMap<String, Double> f_map;
  private HashSet<String> f_set;
  private Optional<String> f_mstring;
  private Optional<String> f_mstring2;
  private Optional<String> f_nstring;
  private Optional<String> f_nstring2;
  private Optional<Long> f_int;
  private Optional<Long> f_int2;
  private Optional<Long> f_int3;

  /* Constructors */

  public S(Pair<Integer, Double> f_pair, Either<String, Integer> f_either, Error<Integer> f_error, HashMap<String, Double> f_map, HashSet<String> f_set, Optional<String> f_mstring, Optional<String> f_mstring2, Optional<String> f_nstring, Optional<String> f_nstring2, Optional<Long> f_int, Optional<Long> f_int2, Optional<Long> f_int3) {
    this.f_pair = Objects.requireNonNull(f_pair);
    this.f_either = Objects.requireNonNull(f_either);
    this.f_error = Objects.requireNonNull(f_error);
    this.f_map = Objects.requireNonNull(f_map);
    this.f_set = Objects.requireNonNull(f_set);
    this.f_mstring = Objects.requireNonNull(f_mstring);
    this.f_mstring2 = Objects.requireNonNull(f_mstring2);
    this.f_nstring = Objects.requireNonNull(f_nstring);
    this.f_nstring2 = Objects.requireNonNull(f_nstring2);
    this.f_int = Objects.requireNonNull(f_int);
    this.f_int2 = Objects.requireNonNull(f_int2);
    this.f_int3 = Objects.requireNonNull(f_int3);
  }

  public S() {
    this.f_pair = Pair.factory(Factories.INT32, Factories.DOUBLE).create();
    this.f_either = Either.factory(Factories.STRING, Factories.INT32).create();
    this.f_error = Error.factory(Factories.INT32).create();
    this.f_map = HashMapHelpers.factory(Factories.STRING, Factories.DOUBLE).create();
    this.f_set = HashSetHelpers.factory(Factories.STRING).create();
    this.f_mstring = MaybeHelpers.factory(Factories.STRING).create();
    this.f_mstring2 = MaybeHelpers.just("sukpeepolup");
    this.f_nstring = Optional.<String>empty();
    this.f_nstring2 = Optional.<String>of("abcde");
    this.f_int = Optional.<Long>empty();
    this.f_int2 = Optional.<Long>of(100L);
    this.f_int3 = Optional.<Long>empty();
  }

  public S(S other) {
    this.f_pair = Pair.factory(Factories.INT32, Factories.DOUBLE).create(other.f_pair);
    this.f_either = Either.factory(Factories.STRING, Factories.INT32).create(other.f_either);
    this.f_error = Error.factory(Factories.INT32).create(other.f_error);
    this.f_map = HashMapHelpers.factory(Factories.STRING, Factories.DOUBLE).create(other.f_map);
    this.f_set = HashSetHelpers.factory(Factories.STRING).create(other.f_set);
    this.f_mstring = MaybeHelpers.factory(Factories.STRING).create(other.f_mstring);
    this.f_mstring2 = MaybeHelpers.factory(Factories.STRING).create(other.f_mstring2);
    this.f_nstring = Factories.nullable(Factories.STRING).create(other.f_nstring);
    this.f_nstring2 = Factories.nullable(Factories.STRING).create(other.f_nstring2);
    this.f_int = Factories.nullable(Factories.INT64).create(other.f_int);
    this.f_int2 = Factories.nullable(Factories.INT64).create(other.f_int2);
    this.f_int3 = Factories.nullable(Factories.INT64).create(other.f_int3);
  }

  /* Accessors and mutators */

  public Pair<Integer, Double> getF_pair() {
    return f_pair;
  }

  public void setF_pair(Pair<Integer, Double> f_pair) {
    this.f_pair = Objects.requireNonNull(f_pair);
  }

  public Either<String, Integer> getF_either() {
    return f_either;
  }

  public void setF_either(Either<String, Integer> f_either) {
    this.f_either = Objects.requireNonNull(f_either);
  }

  public Error<Integer> getF_error() {
    return f_error;
  }

  public void setF_error(Error<Integer> f_error) {
    this.f_error = Objects.requireNonNull(f_error);
  }

  public HashMap<String, Double> getF_map() {
    return f_map;
  }

  public void setF_map(HashMap<String, Double> f_map) {
    this.f_map = Objects.requireNonNull(f_map);
  }

  public HashSet<String> getF_set() {
    return f_set;
  }

  public void setF_set(HashSet<String> f_set) {
    this.f_set = Objects.requireNonNull(f_set);
  }

  public Optional<String> getF_mstring() {
    return f_mstring;
  }

  public void setF_mstring(Optional<String> f_mstring) {
    this.f_mstring = Objects.requireNonNull(f_mstring);
  }

  public Optional<String> getF_mstring2() {
    return f_mstring2;
  }

  public void setF_mstring2(Optional<String> f_mstring2) {
    this.f_mstring2 = Objects.requireNonNull(f_mstring2);
  }

  public Optional<String> getF_nstring() {
    return f_nstring;
  }

  public void setF_nstring(Optional<String> f_nstring) {
    this.f_nstring = Objects.requireNonNull(f_nstring);
  }

  public Optional<String> getF_nstring2() {
    return f_nstring2;
  }

  public void setF_nstring2(Optional<String> f_nstring2) {
    this.f_nstring2 = Objects.requireNonNull(f_nstring2);
  }

  public Optional<Long> getF_int() {
    return f_int;
  }

  public void setF_int(Optional<Long> f_int) {
    this.f_int = Objects.requireNonNull(f_int);
  }

  public Optional<Long> getF_int2() {
    return f_int2;
  }

  public void setF_int2(Optional<Long> f_int2) {
    this.f_int2 = Objects.requireNonNull(f_int2);
  }

  public Optional<Long> getF_int3() {
    return f_int3;
  }

  public void setF_int3(Optional<Long> f_int3) {
    this.f_int3 = Objects.requireNonNull(f_int3);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S other = (S) other0;
    return
      f_pair.equals(other.f_pair) &&
      f_either.equals(other.f_either) &&
      f_error.equals(other.f_error) &&
      f_map.equals(other.f_map) &&
      f_set.equals(other.f_set) &&
      f_mstring.equals(other.f_mstring) &&
      f_mstring2.equals(other.f_mstring2) &&
      f_nstring.equals(other.f_nstring) &&
      f_nstring2.equals(other.f_nstring2) &&
      f_int.equals(other.f_int) &&
      f_int2.equals(other.f_int2) &&
      f_int3.equals(other.f_int3);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f_pair.hashCode();
    _result = _result * 37 + f_either.hashCode();
    _result = _result * 37 + f_error.hashCode();
    _result = _result * 37 + f_map.hashCode();
    _result = _result * 37 + f_set.hashCode();
    _result = _result * 37 + f_mstring.hashCode();
    _result = _result * 37 + f_mstring2.hashCode();
    _result = _result * 37 + f_nstring.hashCode();
    _result = _result * 37 + f_nstring2.hashCode();
    _result = _result * 37 + f_int.hashCode();
    _result = _result * 37 + f_int2.hashCode();
    _result = _result * 37 + f_int3.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private Pair<Integer, Double> f_pair;
    private Either<String, Integer> f_either;
    private Error<Integer> f_error;
    private HashMap<String, Double> f_map;
    private HashSet<String> f_set;
    private Optional<String> f_mstring;
    private Optional<String> f_mstring2;
    private Optional<String> f_nstring;
    private Optional<String> f_nstring2;
    private Optional<Long> f_int;
    private Optional<Long> f_int2;
    private Optional<Long> f_int3;

    public Builder() {
      this.f_pair = null;
      this.f_either = null;
      this.f_error = null;
      this.f_map = null;
      this.f_set = null;
      this.f_mstring = null;
      this.f_mstring2 = MaybeHelpers.just("sukpeepolup");
      this.f_nstring = null;
      this.f_nstring2 = Optional.<String>of("abcde");
      this.f_int = null;
      this.f_int2 = Optional.<Long>of(100L);
      this.f_int3 = Optional.<Long>empty();
    }

    public Builder setF_pair(Pair<Integer, Double> f_pair) {
      this.f_pair = Objects.requireNonNull(f_pair);
      return this;
    }

    public Builder setF_either(Either<String, Integer> f_either) {
      this.f_either = Objects.requireNonNull(f_either);
      return this;
    }

    public Builder setF_error(Error<Integer> f_error) {
      this.f_error = Objects.requireNonNull(f_error);
      return this;
    }

    public Builder setF_map(HashMap<String, Double> f_map) {
      this.f_map = Objects.requireNonNull(f_map);
      return this;
    }

    public Builder setF_set(HashSet<String> f_set) {
      this.f_set = Objects.requireNonNull(f_set);
      return this;
    }

    public Builder setF_mstring(Optional<String> f_mstring) {
      this.f_mstring = Objects.requireNonNull(f_mstring);
      return this;
    }

    public Builder setF_mstring2(Optional<String> f_mstring2) {
      this.f_mstring2 = Objects.requireNonNull(f_mstring2);
      return this;
    }

    public Builder setF_nstring(Optional<String> f_nstring) {
      this.f_nstring = Objects.requireNonNull(f_nstring);
      return this;
    }

    public Builder setF_nstring2(Optional<String> f_nstring2) {
      this.f_nstring2 = Objects.requireNonNull(f_nstring2);
      return this;
    }

    public Builder setF_int(Optional<Long> f_int) {
      this.f_int = Objects.requireNonNull(f_int);
      return this;
    }

    public Builder setF_int2(Optional<Long> f_int2) {
      this.f_int2 = Objects.requireNonNull(f_int2);
      return this;
    }

    public Builder setF_int3(Optional<Long> f_int3) {
      this.f_int3 = Objects.requireNonNull(f_int3);
      return this;
    }

    public S create() {
      Builders.checkFieldInitialized("S", "f_pair", f_pair);
      Builders.checkFieldInitialized("S", "f_either", f_either);
      Builders.checkFieldInitialized("S", "f_error", f_error);
      Builders.checkFieldInitialized("S", "f_map", f_map);
      Builders.checkFieldInitialized("S", "f_set", f_set);
      Builders.checkFieldInitialized("S", "f_mstring", f_mstring);
      Builders.checkFieldInitialized("S", "f_nstring", f_nstring);
      Builders.checkFieldInitialized("S", "f_int", f_int);
      return new S(f_pair, f_either, f_error, f_map, f_set, f_mstring, f_mstring2, f_nstring, f_nstring2, f_int, f_int2, f_int3);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S> FACTORY = new Factory<S>() {
    @Override
    public S create() {
      return new S();
    }

    @Override
    public S create(S other) {
      return new S(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test6", "S");
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
    final Lazy<JsonBinding<Pair<Integer, Double>>> f_pair = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.INT32, JsonBindings.DOUBLE));
    final Lazy<JsonBinding<Either<String, Integer>>> f_either = new Lazy<>(() -> Either.jsonBinding(JsonBindings.STRING, JsonBindings.INT32));
    final Lazy<JsonBinding<Error<Integer>>> f_error = new Lazy<>(() -> Error.jsonBinding(JsonBindings.INT32));
    final Lazy<JsonBinding<HashMap<String, Double>>> f_map = new Lazy<>(() -> HashMapHelpers.jsonBinding(JsonBindings.STRING, JsonBindings.DOUBLE));
    final Lazy<JsonBinding<HashSet<String>>> f_set = new Lazy<>(() -> HashSetHelpers.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<Optional<String>>> f_mstring = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<Optional<String>>> f_mstring2 = new Lazy<>(() -> MaybeHelpers.jsonBinding(JsonBindings.STRING));
    final Lazy<JsonBinding<Optional<String>>> f_nstring = new Lazy<>(() -> JsonBindings.nullable(JsonBindings.STRING));
    final Lazy<JsonBinding<Optional<String>>> f_nstring2 = new Lazy<>(() -> JsonBindings.nullable(JsonBindings.STRING));
    final Lazy<JsonBinding<Optional<Long>>> f_int = new Lazy<>(() -> JsonBindings.nullable(JsonBindings.INT64));
    final Lazy<JsonBinding<Optional<Long>>> f_int2 = new Lazy<>(() -> JsonBindings.nullable(JsonBindings.INT64));
    final Lazy<JsonBinding<Optional<Long>>> f_int3 = new Lazy<>(() -> JsonBindings.nullable(JsonBindings.INT64));
    final Factory<S> _factory = FACTORY;

    return new JsonBinding<S>() {
      @Override
      public Factory<S> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S _value) {
        JsonObject _result = new JsonObject();
        _result.add("f_pair", f_pair.get().toJson(_value.f_pair));
        _result.add("f_either", f_either.get().toJson(_value.f_either));
        _result.add("f_error", f_error.get().toJson(_value.f_error));
        _result.add("f_map", f_map.get().toJson(_value.f_map));
        _result.add("f_set", f_set.get().toJson(_value.f_set));
        _result.add("f_mstring", f_mstring.get().toJson(_value.f_mstring));
        _result.add("f_mstring2", f_mstring2.get().toJson(_value.f_mstring2));
        _result.add("f_nstring", f_nstring.get().toJson(_value.f_nstring));
        _result.add("f_nstring2", f_nstring2.get().toJson(_value.f_nstring2));
        _result.add("f_int", f_int.get().toJson(_value.f_int));
        _result.add("f_int2", f_int2.get().toJson(_value.f_int2));
        _result.add("f_int3", f_int3.get().toJson(_value.f_int3));
        return _result;
      }

      @Override
      public S fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S(
          JsonBindings.fieldFromJson(_obj, "f_pair", f_pair.get()),
          JsonBindings.fieldFromJson(_obj, "f_either", f_either.get()),
          JsonBindings.fieldFromJson(_obj, "f_error", f_error.get()),
          JsonBindings.fieldFromJson(_obj, "f_map", f_map.get()),
          JsonBindings.fieldFromJson(_obj, "f_set", f_set.get()),
          JsonBindings.fieldFromJson(_obj, "f_mstring", f_mstring.get()),
          _obj.has("f_mstring2") ? JsonBindings.fieldFromJson(_obj, "f_mstring2", f_mstring2.get()) : MaybeHelpers.just("sukpeepolup"),
          JsonBindings.fieldFromJson(_obj, "f_nstring", f_nstring.get()),
          _obj.has("f_nstring2") ? JsonBindings.fieldFromJson(_obj, "f_nstring2", f_nstring2.get()) : Optional.<String>of("abcde"),
          JsonBindings.fieldFromJson(_obj, "f_int", f_int.get()),
          _obj.has("f_int2") ? JsonBindings.fieldFromJson(_obj, "f_int2", f_int2.get()) : Optional.<Long>of(100L),
          _obj.has("f_int3") ? JsonBindings.fieldFromJson(_obj, "f_int3", f_int3.get()) : Optional.<Long>empty()
        );
      }
    };
  }
}
