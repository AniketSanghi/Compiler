// A simple interface 
interface Player 
{ 
	int id = 10; 
	int move(); 
} 

interface intfA 
{ 
    void m1(); 
} 
  
interface intfB 
{ 
    void m2(); 
} 
  
class sample implements intfA, intfB 
{ 
    public void m1() 
    { 
        System.out.println("Welcome: inside the method m1"); 
    } 
  
    public void m2() 
    { 
        System.out.println("Welcome: inside the method m2"); 
    }
    public static void main (String[] args) 
    { 
        sample ob1 = new sample(); 
  
        ob1.geekName(); 
        ob1.geekInstitute(); 
    }  
} 