package adl.test;

public class S4<T> {

  public S3<String> f1;
  public S3<T> f2;

  public S4(S3<String> f1, S3<T> f2) {
    this.f1 = f1;
    this.f2 = f2;
  }
}