# 🏀 Predicting NBA Playoff Success Using Machine Learning
This project applies machine learning techniques to predict how much postseason success an NBA team will have based off their regular season preformance. It was developed as a senior undergraduate project focused on sports analytics, statistical modeling, and data-driven prediction.

## 🛠️ Project Summary
This project was built entirely from scratch to explore what regular season performance metrics best predict NBA playoff success. Because no existing dataset matched the project's specific needs, I created one through custom web scraping.
After assembling the dataset, I [EDA](https://github.com/pweave5/Senior_Year_Capstone/blob/main/NBA_Playoff_EDA.ipynb) to uncover patterns and identify potential predictors of postseason performance. Modeling followed, with several approaches tested.

The final model was a stacked ensemble combining:
- Multiple Linear Regression (MLR)
- Random Forest
- Neural Network

When tested on the test set, the ensemble achieved:

- RMSE: 3.55 games

- R²: 0.70 

Interestingly, the model's predictions aligned closely with the betting odds from major sports books—suggesting strong real-world relevance.

For full details on data collection, model development, and evaluation, see the [Final Paper](https://github.com/pweave5/Senior_Year_Capstone/blob/main/Final%20Paper.docx).
## 🚀 Future Improvements and Projects
- Incoporate team playoff experience (e.g., past appearances, veteran presence)
- Implement classification models
    - Predict whether a team will win a head-to-head playoff series

    - Predict championship likelihood


## 📬 Contact
For questions or collaboration, reach out at [pweave03@gmail.com].
