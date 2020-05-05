---
metaTitle: "C# | Implementing Flyweight Design Pattern"
description: "Implementing map in RPG game"
---

# Implementing Flyweight Design Pattern



## Implementing map in RPG game


Flyweight is one of structural design patterns. It is used to decrease the amount of used memory by sharing as much data as possible with similiar objects. This document will teach you how to use Flyweight DP properly.

Let me explain the idea of it to you on a simple example. Imagine you're working on a RPG game and you need to load huge file that contains some characters. For example:

- `#` is grass. You can walk on it.
- `$` is starting point
- `@` is rock. You can't walk on it.
- `%` is treasure chest

Sample of a map:

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

`@############@@@@@######@#$@@@`

`@#############@@@######@###@@@`

`@#######%######@###########@@@`

`@############################@`

`@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@`

Since those objects have similiar characteristic, you don't need to create separate object for each map field. I will show you how to use flyweight.

Let's define an interface which our fields will implement:

```cs
public interface IField
{
    string Name { get; }
    char Mark { get; }
    bool CanWalk { get; }
    FieldType Type { get; }
}

```

Now we can create classes that represent our fields. We also have to identify them somehow (I used an enumeration):

```cs
public enum FieldType
{
    GRASS,
    ROCK,
    START,
    CHEST
}
public class Grass : IField
{
    public string Name { get { return "Grass"; } }
    public char Mark { get { return '#'; } }
    public bool CanWalk { get { return true; } }
    public FieldType Type { get { return FieldType.GRASS; } }
}
public class StartingPoint : IField
{
    public string Name { get { return "Starting Point"; } }
    public char Mark { get { return '$'; } }
    public bool CanWalk { get { return true; } }
    public FieldType Type { get { return FieldType.START; } }
}
public class Rock : IField
{
    public string Name { get { return "Rock"; } }
    public char Mark { get { return '@'; } }
    public bool CanWalk { get { return false; } }
    public FieldType Type { get { return FieldType.ROCK; } }
}
public class TreasureChest : IField
{
    public string Name { get { return "Treasure Chest"; } }
    public char Mark { get { return '%'; } }
    public bool CanWalk { get { return true; } } // you can approach it
    public FieldType Type { get { return FieldType.CHEST; } }
}

```

Like I said, we don't need to create separate instance for each field. We have to create a **repository** of fields. The essence of Flyweight DP is that we dynamically create an object only if we need it and it doesn't exist yet in our repo, or return it if it already exists. Let's write simple class that will handle this for us:

```cs
public class FieldRepository
{
    private List<IField> lstFields = new List<IField>();

    private IField AddField(FieldType type)
    {
        IField f;
        switch(type)
        {
            case FieldType.GRASS: f = new Grass(); break;
            case FieldType.ROCK: f = new Rock(); break;
            case FieldType.START: f = new StartingPoint(); break;
            case FieldType.CHEST:
            default: f = new TreasureChest(); break;
        }
        lstFields.Add(f); //add it to repository
        Console.WriteLine("Created new instance of {0}", f.Name);
        return f;
    }
    public IField GetField(FieldType type)
    {
        IField f = lstFields.Find(x => x.Type == type);
        if (f != null) return f;
        else return AddField(type);
    }
}

```

Great! Now we can test our code:

```cs
public class Program
{
    public static void Main(string[] args)
    {
        FieldRepository f = new FieldRepository();
        IField grass = f.GetField(FieldType.GRASS);
        grass = f.GetField(FieldType.ROCK);
        grass = f.GetField(FieldType.GRASS);       
    }
}

```

The result in the console should be:

> 
Created a new instance of Grass
Created a new instance of Rock


But why grass appears only one time if we wanted to get it twice? That's because first time we call `GetField` grass instance does not exist in our **repository**, so it's created, but next time we need grass it already exist, so we only return it.

