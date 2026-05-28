# Setup skript for å etablere poetry miljø og kernel for jupyter
# Skriv i terminalen: `bash setup.sh`

poetry install
poetry add ipykernel
poetry run python -m ipykernel install --user --name kurs-metode-validere
