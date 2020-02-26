
import java.util.Scanner; 
  
class Test 
{ 
	static void func(int a, int b, int c){
		return a+b+c;
	}
    public static void main( String args[] ) 
    { 
        int value = 15; 
        assert value >= 20 : " Underweight";
        int a = 10, b= 5, c = 10, d = 20;
        func(a, b, c+d); 
        System.out.println("value is "+value); 
    } 
} 
