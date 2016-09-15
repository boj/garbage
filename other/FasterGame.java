class Vector
{
    public float x, y, z;

    public Vector(float x, float y, float z)
    {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    

    public static float getDistance(Vector a, Vector b)
    {
        float sx = a.x - b.x;
        float sy = a.y - b.y;
        float sz = a.z - b.z;
        return (float)Math.sqrt(sx * sx + sy * sy + sz * sz);
    }
}


class Block
{
    public Vector location; //x,y,z within the chunk        
    public String name;
    public int durability;
    public int textureid;
    public boolean breakable;
    public boolean visible;
    public int type;
    public byte id;

    public Block(byte id,Vector location, String name, int durability, int textureid, boolean breakable, boolean visible, int type)
    {
        this.id = id;
        this.location = location;
        this.name = name;
        this.durability = durability;
        this.textureid = textureid;
        this.breakable = breakable;
        this.visible = visible;
        this.type = type;
    }
}

import java.util.ArrayList;


import java.util.ArrayList;
import java.util.Random;

class Entity
{    
    public Vector location; //x,y,z within the chunk
    public String name;
    public int health;
    public Vector speed;
    public enum Type { Zombie, Chicken, Exploder, TallCreepyThing };

    public Entity(Vector location, Type type)
    {
        this.location = location;

        switch (type)
        {
            case Zombie:
                name = "Zombie";
                health = 50;
                speed = new Vector(0.5f, 0.0f, 0.5f); //slow, can't fly
                break;
            case Chicken:
                name = "Chicken";
                health = 25;
                speed = new Vector(0.75f, 0.25f, 0.75f); //can fly a bit
                break;
            case Exploder:
                name = "Exploder";
                health = 75;
                speed = new Vector(0.75f, 0.0f, 0.75f);
                break;
            case TallCreepyThing:
                name = "Tall Creepy Thing";
                health = 500;
                speed = new Vector(1.0f, 1.0f, 1.0f); //does what he wants
                break;
        }

    }

    public void updatePosition()
    {                                
        location.x = location.x + 1*speed.x;
        location.y = location.y + 1*speed.y;
        location.z = location.z + 1*speed.z;
    }

}

import java.util.ArrayList;

import java.util.ArrayList;

class Chunk
{
    private static final int NUM_BLOCKS = 65536; //just like minecraft!
    private static final int NUM_ENTITIES = 1000;

    byte[] blocks;
    ArrayList<Entity> entities;
    public Vector location; //x,y,z within world
    

    public Chunk(Vector location)
    {
        this.location = location;
        //Preallocate the growable List because we are clever!
        blocks = new byte[NUM_BLOCKS];
        for (int i = 0; i < NUM_BLOCKS; i++)
        {
            int id = i%256;            
            blocks[i] = (byte)id;
        }

        entities = new ArrayList<Entity>(NUM_ENTITIES);
        for (int i = 0; i < NUM_ENTITIES / 4; i++)
        {
            // Fancy proc gen initial position equation
            entities.add(new Entity(new Vector(i, i, i), Entity.Type.Chicken));
            entities.add(new Entity(new Vector(i+2, i, i), Entity.Type.Zombie));
            entities.add(new Entity(new Vector(i+3, i, i), Entity.Type.Exploder));
            entities.add(new Entity(new Vector(i+4, i, i), Entity.Type.TallCreepyThing));

        }
    }

    public void processEntities()
    {
        for (Entity entity : entities)
        {
            entity.updatePosition();
        }
    }
}
import java.util.ArrayList;

import java.util.ArrayList;


public class Game {
      
    private static final int CHUNK_COUNT = 100;
    public static Block[] blocks = new Block[256];    
    public static Chunk[] chunks;
    public static Vector playerLocation = new Vector(0, 0, 0);
    public static int chunkCounter = 0;
    static int toRemove = -1;

    static void loadWorld()
    {
        
        for (int i =0; i < blocks.length; i++)
        {
            blocks[i] = new Block((byte)i,new Vector(i,i,i),"Block:"+i,100,1,true,true,1);    
        }
        chunks = new Chunk[CHUNK_COUNT];            
        for (int i = 0; i < CHUNK_COUNT; i++)
        {                
            chunks[i] = new Chunk(new Vector(chunkCounter, 0.0f, 0.0f));
            chunkCounter++;
        }
    }


    static void updateChunks()
    {
        toRemove = -1;
        for (int i = 0; i < chunks.length;i++)
        {
            Chunk chunk = chunks[i];
            chunk.processEntities();
            float chunkDistance = Vector.getDistance(chunk.location, playerLocation);
            if (chunkDistance > CHUNK_COUNT)
            {
                toRemove = i;

            }                
        }

        if (toRemove != -1)
        {            
            chunks[toRemove] = new Chunk(new Vector(chunkCounter, 0.0f, 0.0f));
            chunkCounter++;
        }
        
    }
            


    public static void main(String[] args) {
                                            
        System.out.print("Loading World...");
        long start = System.currentTimeMillis();
        loadWorld();
        long end = System.currentTimeMillis();
        System.out.println("FINISHED!");
        System.out.println("Load Time:" + (end-start));
        //Game Loop, you can never leave
        while (true)
        {
            //check for dead entities
            start = System.nanoTime();

            // Avoid allocating vectors by just mutating the existing one            
            playerLocation.x += 0.1f;
            updateChunks();

            end = System.nanoTime();
            double time = (end-start) / 1000000.0;

            System.out.println("" + time);
            //Lock it at 60FPS
            if (time < 16.0)
            {
                try {
                Thread.sleep((int)(16.0 - time));
                }  catch (Exception e)
                {
                    //help
                }
            }
          
        }
    }            
}