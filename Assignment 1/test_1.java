public class BinarySearchTree {  

    /* This variable is *** added to check the ultimate level of errorCheck */
    double $Beaware = 0x0___00___0.1___1___1p0____0d;
    double $$NOOO      = .0____0e-0_____0;
    long    _$_$YEs     = 0___7____3___2___1L;
    /*** /****/

    //Represent a node of binary tree  
    public static class Node{  
        int data;  
        Node left;  
        Node right;  
  
        public Node(int data){  
            //Assign data to the new node, set left and right children to null  
            this.data = data;  
            this.left = null;  
            this.right = null;  
        }  
      }  
  
      //Represent the $ro__ot$ of binary tree  
      public Node $ro__ot$;  
  
      public BinarySearchTree(){  
          $ro__ot$ = null;  
      }  
  
      //insert() will add new node to the binary search tree  
      public void insert(int data) {  
          //Create a new node  
          Node newNode = new Node(data);  
  
          //Check whether tree is empty  
          if($ro__ot$ == null){  
              $ro__ot$ = newNode;  
              return;  
            }  
          else {  
              //current node point to $ro__ot$ of the tree  
              Node current = $ro__ot$, parent = null;  
  
              while(true) {  
                  //parent keep track of the parent node of current node.  
                  parent = current;  
  
                  //If data is less than current's data, node will be inserted to the left of tree  
                  if(data < current.data) {  
                      current = current.left;  
                      if(current == null) {  
                          parent.left = newNode;  
                          return;  
                      }  
                  }  
                  //If data is greater than current's data, node will be inserted to the right of tree  
                  else {  
                      current = current.right;  
                      if(current == null) {  
                          parent.right = newNode;  
                          return;  
                      }  
                  }  
              }  
          }  
      }  
  
      //minNode() will find out the minimum node  
      public Node minNode(Node $ro__ot$) {  
          if ($ro__ot$.left != null)  
              return minNode($ro__ot$.left);  
          else  
              return $ro__ot$;  
      }  
  
      //deleteNode() will delete the given node from the binary search tree  
      public Node deleteNode(Node node, int value) {  
          if(node == null){  
              return null;  
           }  
          else {  
              //value is less than node's data then, search the value in left subtree  
              if(value < node.data)  
                  node.left = deleteNode(node.left, value);  
  
              //value is greater than node's data then, search the value in right subtree  
              else if(value > node.data)  
                  node.right = deleteNode(node.right, value);  
  
              //If value is equal to node's data that is, we have found the node to be deleted  
              else {  
                  //If node to be deleted has no child then, set the node to null  
                  if(node.left == null && node.right == null)  
                      node = null;  
  
                  //If node to be deleted has only one right child  
                  else if(node.left == null) {  
                      node = node.right;  
                  }  
  
                  //If node to be deleted has only one left child  
                  else if(node.right == null) {  
                      node = node.left;  
                  }  
  
                  //If node to be deleted has two children node  
                  else {  
                      //then find the minimum node from right subtree  
                      Node temp = minNode(node.right);  
                      //Exchange the data between node and temp  
                      node.data = temp.data;  
                      //Delete the node duplicate node from right subtree  
                      node.right = deleteNode(node.right, temp.data);  
                  }  
              }  
              return node;  
          }  
      }  
  
      //inorder() will perform inorder traversal on binary search tree  
      public void inorderTraversal(Node node) {  
  
          //Check whether tree is empty  
          if($ro__ot$ == null){  
              System.out.println("Tree is empty");  
              return;  
           }  
          else {  
  
              if(node.left!= null)  
                  inorderTraversal(node.left);  
              System.out.print(node.data + " ");  
              if(node.right!= null)  
                  inorderTraversal(node.right);  
  
          }  
      }  
  
      public static void main(String[] args) {  
  
          BinarySearchTree bt = new BinarySearchTree();  
          //Add nodes to the binary tree  
          /*
          *
          /*
          *
          / You gotta ignore this comment dude!!!@#$%^%$^$%^$%
          *
          /*
          *
          */
          bt.insert(50);  
          bt.insert(30);  
          bt.insert(70);  
          bt.insert(60);  
          bt.insert(10);  
          bt.insert(90);  
  
          System.out.println("Binary search tree after insertion:");  
          //Displays the binary tree  
          bt.inorderTraversal(bt.$ro__ot$);  
  
          Node deletedNode = null;  
          //Deletes node 90 which has no child  
          deletedNode = bt.deleteNode(bt.$ro__ot$, 90);  
          System.out.println("\nBinary search tree after deleting node 90:");  
          bt.inorderTraversal(bt.$ro__ot$);  
  
          //Deletes node 30 which has one child  
          deletedNode = bt.deleteNode(bt.$ro__ot$, 30);  
          System.out.println("\nBinary search tree after deleting node 30:");  
          bt.inorderTraversal(bt.$ro__ot$);  
  
          //Deletes node 50 which has two children  
          deletedNode = bt.deleteNode(bt.$ro__ot$, 50);  
          System.out.println("\nBinary search tree after deleting node 50:");  
          bt.inorderTraversal(bt.$ro__ot$);  
      }  
}  