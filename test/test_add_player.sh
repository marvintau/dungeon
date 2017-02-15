for i in {1..200}
do
    curl -H "Content-Type: application/json" -X POST -d '' http://localhost:1334/add_new_player
done
