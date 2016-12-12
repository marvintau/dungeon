drop table player_profile;

CREATE TABLE player_profile (
    id serial NOT NULL,	  
    profile jsonb
);

insert into player_profile (profile) values
('{"hp": 2700, "id": "Scarlett", "class": "rouge",
 "agi": 75, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('{"hp": 3400, "id": "Hugh", "class": "hunter", 
 "agi": 40, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 370, "prim_min": 335, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('{"hp": 2300, "id": "Merinda", "class": "mage",
 "agi": 35, "hit": 20, "armor": 2700, "block": 0, "dodge": 20, "resist": 15, "critical": 35,
 "prim_type": "magic", "prim_max": 280, "prim_min": 255, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('{"hp": 3400, "id": "Wallace", "class": "warrior",
 "agi": 50, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}');

select id, profile ->> 'id' from player_profile;