data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Unicycle : Vehicle Pedal
    Tram : Vehicle Electric
    ECar : (battery : Nat) -> Vehicle Electric
    Motorcycle : (fuel : Nat) -> Vehicle Petrol
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 2
wheels Tram = 20
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (ECar battery) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50
refuel Bicycle impossible
refuel Unicycle impossible

recharge : Vehicle Electric -> Vehicle Electric
recharge Tram = Tram 
recharge (ECar battery) = ECar 100
