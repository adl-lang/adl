package org.adl.runtime;

import java.util.ArrayList;

public class Factories
{
  public static final Factory<Integer> integer = new Factory<Integer>() {
    public Integer create() { return 0; }
    public Integer create(Integer other) { return other; }
  };

  public static final Factory<String> string = new Factory<String>() {
    public String create() { return ""; }
    public String create(String other) { return other; }
  };

  public static <T> Factory<ArrayList<T>> arrayList(final Factory<T> factoryT) {
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
