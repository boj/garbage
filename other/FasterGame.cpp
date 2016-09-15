
#include <cstdio>
#include <cstdlib>
#include <chrono>
#include <thread>
#include <cmath>
#include <string>
#include <vector>
#include <array>


using namespace std;
using namespace std::chrono;

class Vector
{
public:
	float x{},y{},z{};
	Vector() = default;
	Vector(float x_,float y_,float z_): x{x_},y{y_},z{z_} { }

	static Vector add(Vector &a,Vector &b)
	{
		return Vector(a.x+b.x,a.y+b.y,a.z+b.z);
	}
	static Vector mul(Vector &a,Vector &b)
	{
		return Vector(a.x*b.x,a.y*b.y,a.z*b.z);
	}
	static Vector sub(Vector &a,Vector &b)
	{
		return Vector(a.x-b.x,a.y-b.y,a.z-b.z);
	}
	static float getDistance(Vector &a,Vector &b)
	{
		Vector s = Vector::sub(a,b);
		float result = std::sqrt(s.x*s.x + s.y*s.y + s.z*s.z);
		return result;
	}
};

class Block
{
public:
	std::string name;
	Vector location;
	int durability;
	int textureid;
	int type;
	unsigned char id;
	bool breakable;
	bool visible;

	Block() = default;
	Block(
		std::string n,
		Vector location,
		unsigned char id,
		int durability,
		int textureid,
		int type,
		bool breakable,
		bool visible):
		name{std::move(n)},
		location{location},
		durability{durability},
		textureid{textureid},
		type{type},
		id{id},
		breakable{breakable},
		visible{visible}
	{
	}
};


class Entity
{
public:
	Vector location;
	Vector speed;
	const char* name;	
	int health;

	enum class Type { Zombie,Chicken,Exploder,TallCreepyThing };
	Entity(Vector,Type);
	Entity() = default;
	~Entity() = default;
	void updatePosition();

};

Entity::Entity (Vector location,Type type)
{
	this->location = location;

	switch(type)
	{
	case Type::Zombie:
		name = "Zombie";
		health = 50;
		speed = Vector(0.5,0.0,0.5);
		break;
	case Type::Chicken:
		name = "Checking";
		health = 25;
		speed = Vector(0.75,0.25,0.75);
		break;
	case Type::Exploder:
		name = "Exploder";
		health = 75;
		speed = Vector(0.75,0.0,0.75);
		break;
	case Type::TallCreepyThing:
		name ="Tall Creepy Thing";
		health = 500;
		speed = Vector(1.0,1.0,1.0);
		break;
	}
}

void Entity::updatePosition()
{
	location = Vector(location.x+1.0f*speed.x,location.y+1.0f*speed.y,location.z+1.0f*speed.z);
}


class Chunk
{
	static constexpr const int NUM_BLOCKS = 65536;
	static constexpr const int NUM_ENTITIES = 1000;

public:
	std::array<unsigned char,NUM_BLOCKS> blocks;
	std::vector<Entity> entities;

	Vector location;
	Chunk(Vector);
	Chunk() = default;
	~Chunk() = default;
	void processEntities();

};

Chunk::Chunk(Vector location)
{
	this->location = location;
	for(int i = 0; i < NUM_BLOCKS;i++)
	{
		blocks[i] = i%256;
	}

	entities.resize(NUM_ENTITIES);
	for(int i = 0; i < NUM_ENTITIES;i=i+4)
	{
		entities[i] = Entity(Vector(i,i,i),Entity::Type::Chicken);
		entities[i+1] = Entity(Vector(i+1,i,i),Entity::Type::Zombie);
		entities[i+2] = Entity(Vector(i+2,i,i),Entity::Type::Exploder);
		entities[i+3] = Entity(Vector(i+3,i,i),Entity::Type::TallCreepyThing);
	}
}

void Chunk::processEntities()
{
	for(auto& entity : entities)
	{
		entity.updatePosition();
	}
}


class Game
{
public:

	static constexpr const int CHUNK_COUNT = 100;
	std::array<Block,256> blocks;
	std::array<Chunk,CHUNK_COUNT> chunks;
	Vector playerLocation;
	vector<int> toRemove;
	int chunkCounter;
	Game();
	void loadWorld();
	void updateChunks();

};

Game::Game()
{
	toRemove = vector<int>(2);
	for(int i = 0; i < 256; i++)
	{
		blocks[i] = Block("Block" + std::to_string(i),
			Vector(i,i,i),i,100,1,1,true,true);
	}

	chunkCounter = 0;
	playerLocation = Vector(0,0,0);
}

void Game::loadWorld() {

	for(int i = 0; i < CHUNK_COUNT;i++)
	{
		chunks[i] = Chunk(Vector(chunkCounter,0.0,0.0));
		chunkCounter++;
	}
}

void Game::updateChunks()
{

	toRemove.clear();
	for(int i = 0; i < CHUNK_COUNT;i++)
	{
		auto& chunk = chunks[i];
		chunk.processEntities();
		float chunkDistance = Vector::getDistance(chunk.location,playerLocation);
		if(chunkDistance > CHUNK_COUNT)
		{
			toRemove.push_back(i);
		}
	}
	
	for (auto i:toRemove)
	{
		chunks[i] = Chunk(Vector(chunkCounter,0.0,0.0));
		chunkCounter++;

	}
}

int main()
{
	auto game = new Game;
	printf("%i\n",sizeof(Game));
	high_resolution_clock::time_point start;
	high_resolution_clock::time_point end;

	printf("loading world...\n");
	start = high_resolution_clock::now();
	game->loadWorld();
	end = high_resolution_clock::now();
	auto duration = duration_cast<milliseconds>(end-start).count();
	printf("load time:%lu\n",duration);
	//spin
	while(1)
	{

		start = high_resolution_clock::now();
		Vector playerMovement = Vector(0.1,0.0,0.0);

		game->playerLocation = Vector::add(playerMovement,game->playerLocation);
		game->updateChunks();

		end = high_resolution_clock::now();

		auto duration = (double)(duration_cast<nanoseconds>(end-start).count() / 1000000.0);

		printf("%f\n",duration);

		if(duration < 16) {
			this_thread::sleep_for(milliseconds((long)(16.0-duration)));
		}
	}
}