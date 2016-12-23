drop table player_profile cascade;

CREATE TABLE player_profile (
    id uuid,	  
    profile jsonb,
    
    unique(id)
);

insert into player_profile (id, profile) values
( '85f6d769-713b-48ad-9163-7ba43b7459c7',
'{"hp": 2700, "id": "Scarlett", "class": "rouge",
 "agi": 75, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('b119a5cb-2311-432c-b6e4-e20be932c714',
'{"hp": 3400, "id": "Hugh", "class": "hunter", 
 "agi": 40, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 370, "prim_min": 335, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('4955c25f-d40d-49b6-9bcd-9ffeebda0052',
 '{"hp": 2300, "id": "Merinda", "class": "mage",
 "agi": 35, "hit": 20, "armor": 2700, "block": 0, "dodge": 20, "resist": 15, "critical": 35,
 "prim_type": "magic", "prim_max": 280, "prim_min": 255, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('9a9d925b-4ec4-41cd-a213-0038b8ec18b7',
 '{"hp": 3400, "id": "Wallace", "class": "warrior",
 "agi": 50, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}');

select id, profile ->> 'id' as name from player_profile;

drop table char_chest;

create table char_chest(
    char_id uuid,
    last_opened_chest int,
    last_opened_time TIMESTAMP,

    foreign key(char_id) references player_profile(id),
    foreign key(last_opened_chest) references chest_spec(chest_id)
);

insert into char_chest(char_id, last_opened_chest, last_opened_time) values
('85f6d769-713b-48ad-9163-7ba43b7459c7', '1', now()),
('b119a5cb-2311-432c-b6e4-e20be932c714', '1', now()),
('4955c25f-d40d-49b6-9bcd-9ffeebda0052', '1', now()),
('9a9d925b-4ec4-41cd-a213-0038b8ec18b7', '1', now());

select * from char_chest ;