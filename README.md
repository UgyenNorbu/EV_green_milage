# 🌿 Bhutan EV Green Distance Tracker

An R Shiny dashboard that estimates how far Bhutan's electric vehicle (EV) fleet has driven, and the petrol, CO₂ emissions and fuel import costs it has avoided.

## ✨ Features

- **Green distance gauge:** a speedometer showing the total estimated kilometres driven by all registered EVs
- **GHG emissions avoided:** tonnes of CO₂ saved compared with equivalent petrol vehicles
- **Fuel import avoided:** value of petrol not imported, in Ngultrum (Nu.)
- **EV population trend:** cumulative fleet growth for cars, buses and two-wheelers
- **Auto-updating:** figures recalculate to the current month each time the app loads

---
## 📊 Input data

The app reads monthly EV registrations, with one row per month:

| Column | Type | Description |
|---|---|---|
| `Month` | text | Month name or abbreviation, e.g. `Jan` |
| `Year` | integer | e.g. `2023` |
| `Cars` | integer | New electric cars registered that month |
| `Buses` | integer | New electric buses registered that month |
| `TwoWheelers` | integer | New electric two-wheelers registered that month |

**Source:** BCTA vehicle registration statistics.

---

## 🧮 Methodology

Each EV is assumed to have been driven a fixed distance every month from its registration month up to the current month.

```
Distance (km)    = vehicles × km per month × months on road
Fuel avoided (L) = distance × fuel use of an equivalent petrol vehicle (L/km)
CO₂ avoided (kg) = fuel avoided × 2.31 kg CO₂/L
Fuel value (Nu.) = fuel avoided × petrol price
```

### Assumptions

| | Cars | Buses | Two-wheelers |
|---|---|---|---|
| Distance (km/month) | 1,200 | 3,300 | 800 |
| Fuel use (L/km) | 0.066 | 0.30 | 0.035 |
| Equivalent (km/L) | ~15.2 | ~3.3 | ~28.6 |

| Parameter | Value |
|---|---|
| Petrol price | Nu. 90 / litre |
| CO₂ emission factor | 2.31 kg CO₂ / litre (IPCC/IEA) |
| Gauge maximum | 100 million km |

These figures are planning-level estimates, not measured values.

- Every vehicle of a type is assumed to drive the same distance each month.
- Vehicles are never retired or deregistered.
- Buses are compared with petrol vehicles, but most ICE buses run on diesel.
- Electricity-generation emissions are not counted. Bhutan's grid is almost entirely hydropower, so this is a reasonable simplification.
- One fixed fuel price is used for all months.

TODO
- Seggregate ICE vehicles by fuel type and calculate the outputs accordingly.
- Make the fuel economy dependent on age of the vehicle
- Use acutal fuel cost in Bhutan