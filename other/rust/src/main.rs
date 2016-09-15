// Optimized game implementation in Rust
extern crate time;


use std::time::Duration;
use time::precise_time_ns;
use std::ops::{Add, Sub, Mul};
use std::{thread};

const BLOCKS_PER_CHUNK: usize = 65535;
const ENTITIES_PER_CHUNK: usize = 1000;
const BLOCK_COUNT: usize = 256;
const CHUNK_COUNT: usize = 100;

fn main() {
    println!("Loading World...");
    let start = precise_time_ns();
    let mut game = Game::new();
    let end = precise_time_ns();
    println!("FINISHED!");
    println!("Load Time: {} milliseconds", (end - start) as f64 * 0.000001);

    loop {
        let start = precise_time_ns();
        let player_movement = Vector::new(0.1, 0.0, 0.0);
        game.player_location = game.player_location + player_movement;
        game.update_chunks();
        let end = precise_time_ns();
        let time = (end - start) as f64 * 0.000001;
        println!("{}", time);
        if time < 16.0 {
            thread::sleep(Duration::from_millis((16.0-time) as u64));
        }
    }
}

/*
Game
*/
struct Game {
    // Nothing actually uses the blocks right now, but don't warn
    // With some unsafe, we could un-box these, but it's probably not worth it
    #[allow(dead_code)]
    blocks: Box<[Block]>,
    chunks: Box<[Chunk]>,
    player_location: Vector,
    chunk_counter: u32,
}

impl Game {
    fn new() -> Game {
        let chunks = (0..CHUNK_COUNT)
            .map(|i| Chunk::new(Vector::new(i as f32, 0.0, 0.0)))
            .collect::<Vec<Chunk>>()
            .into_boxed_slice();

        let blocks = (0..BLOCK_COUNT)
            .map(|i| {
                Block {
                    id: i as u8,
                    name: format!("Block{}", i),
                    location: Vector::new(i as f32, i as f32, i as f32),
                    durability: 100,
                    textureid: 1,
                    block_type: 1,
                    breakable: true,
                    visible: true,
                }
            })
            .collect::<Vec<Block>>()
            .into_boxed_slice();

        Game {
            blocks: blocks,
            chunks: chunks,
            player_location: Vector::new(0.0, 0.0, 0.0),
            chunk_counter: CHUNK_COUNT as u32,
        }
    }

    fn update_chunks(&mut self) {
        for chunk in self.chunks.iter_mut() {
            let chunk_distance = Vector::get_distance(chunk.location, self.player_location);
            if chunk_distance > CHUNK_COUNT as f32 {
                chunk.reset();
                chunk.location.x = self.chunk_counter as f32;
                self.chunk_counter += 1;
            }
            
            // Don't process entities on chunks which we're going to reset anyway
            chunk.process_entites();
        }
    }
}

/*
Chunk
*/
struct Chunk {
    block_ids: [u8; BLOCKS_PER_CHUNK],
    // Should be a Vec, if it's possible to have less than ENTITIES_PER_CHUNK
    entities: Vec<Entity>,
    location: Vector
}

impl Chunk {
    fn new(location: Vector) -> Chunk {
        let mut result = Chunk {
            block_ids: [0; BLOCKS_PER_CHUNK],
            entities: Vec::with_capacity(ENTITIES_PER_CHUNK),
            location: location,
        };
        result.reset();
        result
    }

    fn process_entites(&mut self) {
        for entity in self.entities.iter_mut() {
            entity.update_position();
        }
    }

    fn reset(&mut self) {
        for (i, block_id) in self.block_ids.iter_mut().enumerate() {
            *block_id = (i % BLOCK_COUNT) as u8;
        }
        self.entities.clear();
        for i in 0..(ENTITIES_PER_CHUNK / 4) {
            let converted_i = i as f32;
            self.entities.push(Entity::new(
                Vector::new(converted_i, converted_i, converted_i),
                EntityType::Chicken
            ));
            self.entities.push(Entity::new(
                Vector::new(converted_i + 1.0, converted_i, converted_i),
                EntityType::Zombie
            ));
            self.entities.push(Entity::new(
                Vector::new(converted_i + 2.0, converted_i, converted_i),
                EntityType::Exploder
            ));
            self.entities.push(Entity::new(
                Vector::new(converted_i + 3.0, converted_i, converted_i),
                EntityType::TallCreepyThing
            ));
        }
    }
}

/*
Entity
*/
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum EntityType {
    Zombie,
    Chicken,
    Exploder,
    TallCreepyThing
}

#[derive(Debug, Clone)]
struct Entity {
    location: Vector,
    name: String,
    health: i32,
    speed: Vector,
}

impl Entity {
    fn new(location: Vector, entity_type: EntityType) -> Entity {
        match entity_type {
            EntityType::Zombie => Entity {
                location: location,
                name: "Zombie".to_string(),
                health: 50,
                speed: Vector::new(0.5, 0.0, 0.5) // slow, can't fly
            },
            EntityType::Chicken => Entity {
                location: location,
                name: "Chicken".to_string(),
                health: 25,
                speed: Vector::new(0.75, 0.25, 0.75) // can fly a bit
            },
            EntityType::Exploder => Entity {
                location: location,
                name: "Exploder".to_string(),
                health: 75,
                speed: Vector::new(0.75, 0.0, 0.75)
            },
            EntityType::TallCreepyThing => Entity {
                location: location,
                name: "Tall Creepy Thing".to_string(),
                health: 500,
                speed: Vector::new(1.0, 1.0, 1.0) // does what he wants
            }
        }
    }

    fn update_position(&mut self) {
        self.location = self.location + self.speed;
    }
}

/*
Block
*/
#[derive(Debug, Clone, PartialEq)]
struct Block {
    id: u8,
    name: String,
    location: Vector,
    durability: i32,
    textureid: usize,
    breakable: bool,
    visible: bool,
    block_type: i32
}

/*
Vector
*/
#[test]
fn test_vector() {
    let a = Vector::new(3.0, 2.0, 1.0);
    let b = Vector::new(6.5, 3.0, 5.5);
    assert_eq!(a + b, Vector::new(9.5, 5.0, 6.5));

    let a = Vector::new(3.0, 2.0, 1.0);
    let b = Vector::new(2.0, 4.0, 5.0);
    assert_eq!(a * b, Vector::new(6.0, 8.0, 5.0));

    let a = Vector::new(3.0, 3.0, 1.0);
    let b = Vector::new(2.0, 1.0, 2.0);
    assert_eq!(a - b, Vector::new(1.0, 2.0, -1.0));
}

#[derive(PartialEq, Debug, Clone, Copy)]
struct Vector {
    x: f32, // floats in C# are single-precision, so f32 is used here instead of f64
    y: f32,
    z: f32
}

impl Vector {
    fn new(x: f32, y: f32, z: f32) -> Vector {
        Vector {
            x: x,
            y: y,
            z: z
        }
    }
    fn get_distance(a: Vector, b: Vector) -> f32 {
        let s = a - b;
        (s.x * s.x + s.y * s.y + s.z * s.z).sqrt()
    }
}

impl Add for Vector {
    type Output = Vector;

    fn add(self, _rhs: Vector) -> Vector {
        Vector {
            x: self.x + _rhs.x,
            y: self.y + _rhs.y,
            z: self.z + _rhs.z
        }
    }
}

impl Sub for Vector {
    type Output = Vector;

    fn sub(self, _rhs: Vector) -> Vector {
        Vector {
            x: self.x - _rhs.x,
            y: self.y - _rhs.y,
            z: self.z - _rhs.z
        }
    }
}

impl Mul for Vector {
    type Output = Vector;

    fn mul(self, _rhs: Vector) -> Vector {
        Vector {
            x: self.x * _rhs.x,
            y: self.y * _rhs.y,
            z: self.z * _rhs.z
        }
    }
}