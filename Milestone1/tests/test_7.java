class ArrayStuff
{ 
	public static void main (String[] args) 
	{		 
		int[] arr; 
			
		arr = new int[5]; 
			
		arr[0] = 10; 
			
		arr[1] = 20; 
			
		arr[2] = 30; 
		arr[3] = 40; 
		arr[4] = 50;

		boolean[][][] moredims;
		moredims = new boolean [6][7][8];
		//wrong statement but used just to demonstrate the name node's utility
		moredims.a.b.c[0][2][4]=10; 
	}
		
} 
