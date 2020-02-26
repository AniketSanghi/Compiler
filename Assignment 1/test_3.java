package com.sanfoundry.hardgraph;
 
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.Stack;
 
class Bag<Item> implements Iterable<Item>
{
    private int        N;    // number of elements in bag
    private Node<Item> first; // beginning of bag
 
    // helper linked list class
    private static class Node<Item>
    {
        private Item       item;
        private Node<Item> next;
    }
 
    public Bag()
    {
        first = null;
        N = 0;
    }
 
    public boolean isEmpty()
    {
        return first == null;
    }
 
    public int size()
    {
        return N;
    }
 
    public void add(Item item)
    {
        Node<Item> oldfirst = first;
        first = new Node<Item>();
        first.item = item;
        first.next = oldfirst;
        N++;
    }
 
    public Iterator<Item> iterator()
    {
        return new ListIterator<Item>(first);
    }
 
    // an iterator, doesn't implement remove() since it's optional
    @SuppressWarnings("hiding")
    private class ListIterator<Item> implements Iterator<Item>
    {
        private Node<Item> current;
 
        public ListIterator(Node<Item> first)
        {
            current = first;
        }
 
        public boolean hasNext()
        {
            return current != null;
        }
 
        public void remove()
        {
            throw new UnsupportedOperationException();
        }
 
        public Item next()
        {
            if (!hasNext())
                throw new NoSuchElementException();
            Item item = current.item;
            current = current.next;
            return item;
        }
    }
}
 
class BridgeGraph
{
    private final int      V;
    private int            E;
    private Bag<Integer>[] adj;
 
    @SuppressWarnings("unchecked")
    public BridgeGraph(int V)
    {
        if (V < 0)
            throw new IllegalArgumentException(
                    "Number of vertices must be nonnegative");
        this.V = V;
        this.E = 0;
        adj = (Bag<Integer>[]) new Bag[V];
        for (int v = 0; v < V; v++)
        {
            adj[v] = new Bag<Integer>();
        }
        System.out.println("Enter the number of edges: ");
        Scanner sc = new Scanner(System.in);
        int E = sc.nextInt();
        if (E < 0)
        {
            sc.close();
            throw new IllegalArgumentException(
                    "Number of edges must be nonnegative");
        }
        for (int i = 0; i < E; i++)
        {
            int v = sc.nextInt();
            int w = sc.nextInt();
            addEdge(v, w);
        }
        sc.close();
    }
 
    public BridgeGraph(BridgeGraph G)
    {
        this(G.V());
        this.E = G.E();
        for (int v = 0; v < G.V(); v++)
        {
            // reverse so that adjacency list is in same order as original
            Stack<Integer> reverse = new Stack<Integer>();
            for (int w : G.adj[v])
            {
                reverse.push(w);
            }
            for (int w : reverse)
            {
                adj[v].add(w);
            }
        }
    }
 
    public int V()
    {
        return V;
    }
 
    public int E()
    {
        return E;
    }
 
    public void addEdge(int v, int w)
    {
        if (v < 0 || v >= V)
            throw new IndexOutOfBoundsException();
        if (w < 0 || w >= V)
            throw new IndexOutOfBoundsException();
        E++;
        adj[v].add(w);
        adj[w].add(v);
    }
 
    public Iterable<Integer> adj(int v)
    {
        if (v < 0 || v >= V)
            throw new IndexOutOfBoundsException();
        return adj[v];
    }
 
    public String toString()
    {
        StringBuilder s = new StringBuilder();
        String NEWLINE = System.getProperty("line.separator");
        s.append(V + " vertices, " + E + " edges " + NEWLINE);
        for (int v = 0; v < V; v++)
        {
            s.append(v + ": ");
            for (int w : adj[v])
            {
                s.append(w + " ");
            }
            s.append(NEWLINE);
        }
        return s.toString();
    }
}
 
public class BridgesinGraph
{
    private int   bridges; // number of bridges
    private int   cnt;    // counter
    private int[] pre;    // pre[v] = order in which dfs examines v
    private int[] low;    // low[v] = lowest preorder of any vertex connected
                           // to v
 
    public BridgesinGraph(BridgeGraph G)
    {
        low = new int[G.V()];
        pre = new int[G.V()];
        for (int v = 0; v < G.V(); v++)
            low[v] = -1;
        for (int v = 0; v < G.V(); v++)
            pre[v] = -1;
        for (int v = 0; v < G.V(); v++)
            if (pre[v] == -1)
                dfs(G, v, v);
    }
 
    public int components()
    {
        return bridges + 1;
    }
 
    private void dfs(BridgeGraph G, int u, int v)
    {
        pre[v] = cnt++;
        low[v] = pre[v];
        for (int w : G.adj(v))
        {
            if (pre[w] == -1)
            {
                dfs(G, v, w);
                low[v] = Math.min(low[v], low[w]);
                if (low[w] == pre[w])
                {
                    System.out.println(v + "-" + w + " is a bridge");
                    bridges++;
                }
            }
            // update low number - ignore reverse of edge leading to v
            else if (w != u)
                low[v] = Math.min(low[v], pre[w]);
        }
    }
 
    public static void main(String[] args)
    {
        Scanner sc = new Scanner(System.in);
        System.out.println("Enter the number of vertices: ");
        BridgeGraph G = new BridgeGraph(sc.nextInt());
        System.out.println(G);
        BridgesinGraph bridge = new BridgesinGraph(G);
        System.out
                .println("Edge connected components = " + bridge.components());
        sc.close();
    }
}