package org.adl.runtime;

import java.util.ArrayList;

public class Factories
{
  public static final Factory<Integer> IntegerFactory = new Factory<Integer>() {
    public Integer create() { return 0; }
    public Integer create(Integer other) { return other; }
  };

  public static final Factory<String> StringFactory = new Factory<String>() {
    public String create() { return ""; }
    public String create(String other) { return other; }
  };

  public static final Factory<Double> DoubleFactory = new Factory<Double>() {
    public Double create() { return 0.0; }
    public Double create(Double other) { return other; }
  };

  public static <T> Factory<ArrayList<T>> ArrayListFactory(final Factory<T> factoryT) {
    return new Factory<ArrayList<T>>() {
      public ArrayList<T> create() {
        return new java.util.ArrayList<T>();
      }

      public ArrayList<T> create(ArrayList<T> other) {
        ArrayList<T> result = new ArrayList<T>();
        for (T v : other) {
          result.add( factoryT.create(v) );
        }
        return result;
      }
    };
  }
};
