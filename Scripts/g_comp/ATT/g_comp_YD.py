def summarize_mergeid_completeness(df, outcome_vars, label=None):
    label = label or ", ".join(outcome_vars)
    original_mergeids = set(df['mergeid'].unique())
    
    complete_mergeids = set(
        df.groupby('mergeid')
          .filter(lambda g: not g[outcome_vars].isnull().any().any())['mergeid']
          .unique()
    )
    
    dropped = len(original_mergeids) - len(complete_mergeids)
    
    print(f"[{label}]")
    print(f"Original mergeids: {len(original_mergeids)}")
    print(f"Complete mergeids (no missing values in Y): {len(complete_mergeids)}")
    print(f"Number of unique mergeids dropped: {dropped}\n")
    
    return complete_mergeids


def get_valid_df(df, complete_mergeids, label, T):
    df_clean = df[df['mergeid'].isin(complete_mergeids)]
    
    valid_mergeids = (
        df_clean
        .groupby("mergeid")
        .filter(lambda g: g["t_age"].nunique() == T)["mergeid"]
        .unique()
    )
    
    df_valid = df_clean[df_clean['mergeid'].isin(valid_mergeids)]
    
    print(f"[{label}]")
    print(f"Mergeids with complete outcome and {T} unique ages: {len(valid_mergeids)}")
    
    return df_valid

# Feature column generator
def get_feature_cols(df, context="outcome and med"):
    """
    Returns a list of feature columns for modeling,
    excluding regime reference countries and/or leakage-prone variables,
    depending on the specified context.
    """
    context = context.lower().strip()

    context_exclusions = {
        "outcome and med": ["mod_country_italy"],
        "outcome and cor": ["mod_country_germany"],
        "outcome and scan": ["mod_country_sweden"],
        "tv_covar and med": ["mod_country_italy", "dt_n_years_disease_dic"],
        "tv_covar and cor": ["mod_country_germany", "dt_n_years_disease_dic"],
        "tv_covar and scan": ["mod_country_sweden", "dt_n_years_disease_dic"],
    }

    exclude_vars = context_exclusions.get(context, [])
    if context not in context_exclusions:
        print(f"Warning: Unrecognized context '{context}', using all features.")
    else:
        print(f"[get_feature_cols] Context: {context} | Exclude: {exclude_vars}")

    selected_cols = [
        col for col in df.columns
        if (
            col.startswith("pre_") or 
            col.startswith("dt_n_years") or
            col.startswith("trt_") or
            col.startswith("mod_country")
        ) and col not in exclude_vars
    ]

    if "dt_n_years_disease_dic" in selected_cols:
        print("⚠️ dt_n_years_disease_dic IS STILL INCLUDED")
    else:
        print("✅ dt_n_years_disease_dic successfully excluded")

    return selected_cols



import numpy as np
import torch

# Step 5: Convert to (N, T, D) tensor
def convert_df_to_X(df_clean, feature_cols, N, T):
    D = len(feature_cols)
    X_array = np.zeros((N, T, D), dtype=np.float32)
    
    for i, (mergeid, group) in enumerate(df_clean.groupby("mergeid")):
        group_sorted = group.sort_values("t_age")
        if len(group_sorted) != T or group_sorted["t_age"].nunique() != T:
            raise ValueError(f"Mergeid {mergeid} does not have {T} time points")
        X_array[i] = group_sorted[feature_cols].to_numpy()

    return torch.tensor(X_array)

def extract_y_tensor(df, col_name):
    y_df = (
        df[["mergeid", col_name]]
        .drop_duplicates("mergeid")
        .sort_values("mergeid")
    )
    return torch.tensor(y_df[col_name].values, dtype=torch.float32).unsqueeze(1)


import torch
import torch.nn.functional as F

# Extract treatment features as well as covariates 
def extract_features(X, feature_cols):
    """
    Extract trajectory-based features from longitudinal data X of shape [N, T, D].

    Features include:
    - Cumulative exposure to full-time and part-time employment
    - Cohabitation type summaries and transitions
    - Timing and duration of states
    - Volatility (entropy) and most recent status
    - Covariates: all 'pre_' and 'dt_n_years' variables, and welfare regime dummies

    Args:
        X (torch.Tensor): Long format tensor of shape [N, T, D]
        feature_cols (list of str): List of column names corresponding to X's last dimension

    Returns:
        torch.Tensor: Feature matrix of shape [N, F] for downstream use
    """
    N, T, D = X.shape
    fn_idx = {name: i for i, name in enumerate(feature_cols)}

    # --- Treatment variable groups
    work_vars = [
        "trt_full_time_employment",
        "trt_part_time_employment",
        "trt_in_education",
        "trt_not_working"
    ]
    cohab_vars = [
        "trt_not_cohabit_0_children",
        "trt_cohabit_0_children",
        "trt_not_cohabit_with_children",
        "trt_cohabit_with_children"
    ]

    work_idxs = [fn_idx[v] for v in work_vars]
    cohab_idxs = [fn_idx[v] for v in cohab_vars]

    work_treat = X[:, :, work_idxs]     # [N, T, 4]
    cohab_treat = X[:, :, cohab_idxs]   # [N, T, 4]

    # --- Cumulative and dominant exposure
    # Dominant work type
    cum_work_type = work_treat.sum(dim=1)             # [N, 4]
    dominant_work = torch.argmax(cum_work_type, dim=1)  # [N]
    dominant_work_oh = F.one_hot(dominant_work, num_classes=4).float()  # [N, 4]

    # Cumulative full-time and part-time 
    cum_work = work_treat[:, :, [0, 1]].sum(dim=1)  # [N, 2]

    # Dominant family type
    cum_cohab = cohab_treat.sum(dim=1)             # [N, 4]
    dominant_cohab = torch.argmax(cum_cohab, dim=1)  # [N]
    dominant_cohab_oh = F.one_hot(dominant_cohab, num_classes=4).float()  # [N, 4]

    # Cumulative years of cohabitation or having children 
    total_cohab_years = (cum_cohab[:, 1] + cum_cohab[:, 3]).unsqueeze(1)
    total_not_cohab_years = (cum_cohab[:, 0] + cum_cohab[:, 2]).unsqueeze(1)
    total_children_years = (cum_cohab[:, 2] + cum_cohab[:, 3]).unsqueeze(1)
    total_no_children_years = (cum_cohab[:, 0] + cum_cohab[:, 1]).unsqueeze(1)

    # --- Transition counts
    def count_transitions(onehot_tensor):
        labels = torch.argmax(onehot_tensor, dim=2)
        transitions = (labels[:, 1:] != labels[:, :-1]).sum(dim=1)
        return transitions.unsqueeze(1)

    trans_work = count_transitions(work_treat)
    trans_cohab = count_transitions(cohab_treat)

    # --- Timing of first events
    def first_occurrence(onehot_tensor, class_idx):
        labels = torch.argmax(onehot_tensor, dim=2)
        first = (labels == class_idx).float()
        idxs = torch.arange(T, device=X.device).view(1, -1)
        first_time = (first * idxs).masked_fill(first == 0, T).min(dim=1).values
        return first_time.unsqueeze(1)

    first_fulltime = first_occurrence(work_treat, class_idx=0)
    first_cohabit = first_occurrence(cohab_treat, class_idx=1)

    # --- Max spell duration
    def max_duration(onehot_tensor):
        labels = torch.argmax(onehot_tensor, dim=2)
        max_durs = []
        for row in labels:
            diffs = torch.cat([torch.tensor([1], device=row.device), (row[1:] != row[:-1]).int()])
            spell_ids = torch.cumsum(diffs, dim=0)
            _, counts = torch.unique(spell_ids, return_counts=True)
            max_durs.append(counts.max())
        return torch.stack(max_durs).unsqueeze(1)

    longest_work_spell = max_duration(work_treat)
    longest_cohab_spell = max_duration(cohab_treat)

    # --- Most recent status (one-hot)
    last_work_label = torch.argmax(work_treat[:, -1, :], dim=1)        # [N]
    last_cohab_label = torch.argmax(cohab_treat[:, -1, :], dim=1)      # [N]
    last_work_oh = F.one_hot(last_work_label, num_classes=4).float()   # [N, 4]
    last_cohab_oh = F.one_hot(last_cohab_label, num_classes=4).float() # [N, 4]

    # --- Entropy of trajectory
    def normalized_entropy(onehot_tensor):
        labels = torch.argmax(onehot_tensor, dim=2)
        entropies = []
        for row in labels:
            counts = torch.bincount(row, minlength=onehot_tensor.size(2)).float()
            probs = counts / counts.sum()
            log_probs = torch.log2(probs + 1e-8)
            entropy = -(probs * log_probs).sum()
            entropies.append(entropy / torch.log2(torch.tensor(onehot_tensor.size(2), dtype=torch.float)))
        return torch.stack(entropies).unsqueeze(1)

    entropy_work = normalized_entropy(work_treat)
    entropy_cohab = normalized_entropy(cohab_treat)

    # --- Time-invariant covariates
    covars_idxs = [
        i for i, name in enumerate(feature_cols)
        if name.startswith("pre_")
            or name.startswith("dt_n_years")
            or name.startswith("mod_welfare_regime")
    ]
    X_covars = X[:, 0, covars_idxs]  # [N, D_covars]

    # --- Final feature matrix
    return torch.cat([
        cum_work,               # [N, 2]
        total_cohab_years,      # [N, 1]
        total_not_cohab_years,  # [N, 1]
        total_children_years,   # [N, 1]
        total_no_children_years,# [N, 1]
        dominant_work_oh,  # [N, 4] one-hot
        dominant_cohab_oh,      # [N, 4] one-hot
        trans_work,             # [N, 1]
        trans_cohab,            # [N, 1]
        first_fulltime,         # [N, 1]
        first_cohabit,          # [N, 1]
        longest_work_spell,     # [N, 1]
        longest_cohab_spell,    # [N, 1]
        last_work_oh,           # [N, 4] one-hot
        last_cohab_oh,          # [N, 4] one-hot
        entropy_work,           # [N, 1]
        entropy_cohab,          # [N, 1]
        X_covars                # [N, D_covars]
    ], dim=1)



import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier, StackingClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from xgboost import XGBClassifier
from sklearn.model_selection import RandomizedSearchCV
from scipy.stats import loguniform, randint, uniform
from sklearn.model_selection import train_test_split
from sklearn.metrics import (
    accuracy_score,
    roc_auc_score,
    ConfusionMatrixDisplay,
    roc_curve
)
import pandas as pd
import os
import json
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score, roc_auc_score, roc_curve, ConfusionMatrixDisplay

def train_and_evaluate_with_tuning(X, y, label, save_dir="model_outputs"):
    import numpy as np
    from sklearn.model_selection import train_test_split, RandomizedSearchCV
    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler
    from sklearn.linear_model import LogisticRegression
    from sklearn.ensemble import RandomForestClassifier, StackingClassifier
    from xgboost import XGBClassifier
    from scipy.stats import loguniform, randint, uniform

    # Create save directory if not exists
    os.makedirs(save_dir, exist_ok=True)

    y_np = y.squeeze().numpy()
    ratio = float((y_np == 0).sum()) / (y_np == 1).sum()

    # Base learners
    lasso_pipe = make_pipeline(
        StandardScaler(),
        LogisticRegression(
            penalty='l1', solver='saga', class_weight='balanced', max_iter=1000, random_state=42
        )
    )
    rf = RandomForestClassifier(class_weight='balanced', random_state=42)
    xgb = XGBClassifier(
        scale_pos_weight=ratio, use_label_encoder=False, eval_metric='logloss', random_state=42
    )

    # Hyperparameter spaces
    param_dist_lasso = {'logisticregression__C': loguniform(0.01, 5)}
    param_dist_rf = {
        'n_estimators': randint(100, 300),
        'max_depth': randint(5, 20),
        'min_samples_split': randint(2, 10)
    }
    param_dist_xgb = {
        'n_estimators': randint(100, 300),
        'max_depth': randint(5, 10),
        'learning_rate': uniform(0.01, 0.1),
        'subsample': uniform(0.7, 0.3)
    }

    # Train/test split
    X_train, X_test, y_train, y_test = train_test_split(X, y_np, test_size=0.3, random_state=42)

    # Hyperparameter tuning
    lasso_cv = RandomizedSearchCV(lasso_pipe, param_distributions=param_dist_lasso, n_iter=10, cv=5, scoring='roc_auc', random_state=42)
    rf_cv = RandomizedSearchCV(rf, param_distributions=param_dist_rf, n_iter=10, cv=5, scoring='roc_auc', random_state=42)
    xgb_cv = RandomizedSearchCV(xgb, param_distributions=param_dist_xgb, n_iter=10, cv=5, scoring='roc_auc', random_state=42)

    lasso_cv.fit(X_train, y_train)
    rf_cv.fit(X_train, y_train)
    xgb_cv.fit(X_train, y_train)

    best_models = {"lasso": lasso_cv, "rf": rf_cv, "xgb": xgb_cv}

    # Stacking
    estimators = [
        ('lasso', lasso_cv.best_estimator_),
        ('rf', rf_cv.best_estimator_),
        ('xgb', xgb_cv.best_estimator_)
    ]
    final_est = LogisticRegression(penalty='l2', C=10.0, solver='lbfgs', class_weight='balanced', max_iter=1000, random_state=42)

    y_model = StackingClassifier(estimators=estimators, final_estimator=final_est, passthrough=False, cv=5)
    y_model.fit(X_train, y_train)

    # Evaluation
    y_train_pred = y_model.predict(X_train)
    y_test_pred = y_model.predict(X_test)
    y_test_proba = y_model.predict_proba(X_test)[:, 1]

    train_acc = accuracy_score(y_train, y_train_pred)
    test_acc = accuracy_score(y_test, y_test_pred)
    test_auc = roc_auc_score(y_test, y_test_proba)

    print(f"\n=== {label} ===")
    print("Train Accuracy:", train_acc)
    print("Test Accuracy:", test_acc)
    print("Test AUC:", test_auc)

    # Save metrics
    metrics = {
        "label": label,
        "train_accuracy": train_acc,
        "test_accuracy": test_acc,
        "test_auc": test_auc
    }
    metrics_df = pd.DataFrame([metrics])
    metrics_df.to_csv(os.path.join(save_dir, f"{label}_metrics.csv"), index=False)

    # Save confusion matrix
    disp = ConfusionMatrixDisplay.from_estimator(y_model, X_test, y_test)
    plt.title(f"Confusion Matrix: {label}")
    plt.savefig(os.path.join(save_dir, f"{label}_confusion_matrix.png"))
    plt.close()

    # Save ROC curve
    fpr, tpr, _ = roc_curve(y_test, y_test_proba)
    plt.plot(fpr, tpr, label=f"AUC = {test_auc:.2f}")
    plt.plot([0, 1], [0, 1], 'k--')
    plt.xlabel("False Positive Rate")
    plt.ylabel("True Positive Rate")
    plt.title(f"ROC Curve: {label}")
    plt.legend()
    plt.savefig(os.path.join(save_dir, f"{label}_roc_curve.png"))
    plt.close()

    return y_model, best_models




import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
from sklearn.inspection import permutation_importance

# --- 1. Feature importance from base learners
def plot_base_model_feature_importance(X, base_models, feature_names, label, output_dir="intermediate"):
    os.makedirs(output_dir, exist_ok=True)

    for name, model in base_models.items():
        print(f"\n=== {name.upper()} Feature Importance ===")

        if name == "lasso":
            pipe = model.best_estimator_
            if hasattr(pipe.named_steps["logisticregression"], "coef_"):
                importances = np.abs(pipe.named_steps["logisticregression"].coef_.flatten())
            else:
                print("No coef_ found in logisticregression.")
                continue

        elif name in ["rf", "xgb"]:
            estimator = model.best_estimator_
            if hasattr(estimator, "feature_importances_"):
                importances = estimator.feature_importances_
            else:
                print(f"No feature_importances_ found in {name.upper()}.")
                continue

        else:
            print(f"No importance method for model: {name}")
            continue

        df = pd.DataFrame({'feature': feature_names, 'importance': importances})
        df = df.sort_values(by='importance', ascending=False).head(20)
        print(df)

        # Plot and save
        df.plot.barh(x='feature', y='importance', title=f"{name.upper()} Feature Importance", figsize=(7, 5))
        plt.gca().invert_yaxis()
        plt.tight_layout()
        plot_filename = f"{output_dir}/{label.lower().replace(' ', '_').replace('–', '-')}_{name}_feature_importance.png"
        plt.savefig(plot_filename)
        plt.show()

# --- 2. Permutation importance from Super Learner
def plot_super_learner_permutation_importance(X, y, model, feature_names, label, output_dir="intermediate"):
    os.makedirs(output_dir, exist_ok=True)

    print("\n=== Permutation Importance (Super Learner) ===")
    perm_result = permutation_importance(model, X, y, scoring='roc_auc', n_repeats=5, random_state=42)

    perm_df = pd.DataFrame({
        'feature': feature_names,
        'importance_mean': perm_result.importances_mean,
        'importance_std': perm_result.importances_std
    }).sort_values(by='importance_mean', ascending=False)

    print(perm_df.head(20))

    # Plot and save
    perm_df.head(20).plot.barh(x='feature', y='importance_mean',
                               title="Permutation Importance (Super Learner)", figsize=(8, 10))
    plt.gca().invert_yaxis()
    plt.tight_layout()
    plot_filename = f"{output_dir}/{label.lower().replace(' ', '_').replace('–', '-')}_super_learner_permutation_importance.png"
    plt.savefig(plot_filename)
    plt.show()


# Generate feature names for plotting 
def create_feature_df(X_features_tensor, feature_cols, dataset_label=""):
    # Base treatment-derived trajectory feature names
    treatment_feature_names = [
        "cum_full_time", "cum_part_time",
        "years_cohab", "years_not_cohab", "years_with_children", "years_without_children",

        "dominant_work_full_time", "dominant_work_part_time", 
        "dominant_work_education", "dominant_work_not_working",

        "dominant_nc_no_child", "dominant_c_no_child", 
        "dominant_nc_with_child", "dominant_c_with_child",

        "trans_work", "trans_cohab",
        "first_fulltime", "first_cohabit",
        "longest_work_spell", "longest_cohab_spell",

        "last_work_full_time", "last_work_part_time", 
        "last_work_education", "last_work_not_working",

        "last_cohab_nc_no_child", "last_cohab_c_no_child",
        "last_cohab_nc_with_child", "last_cohab_c_with_child",

        "entropy_work", "entropy_cohab"
    ]

    # Time-invariant covariates from provided feature list
    exclude_vars = ["pre_sex", "mod_welfare_regime_corporatist"]
    covar_names = [
        name for name in feature_cols
        if name.startswith("pre_") or name.startswith("dt_n_years") or name.startswith("mod_welfare_regime")
        and name not in exclude_vars
    ]

    # Final feature names
    all_feature_names = treatment_feature_names + covar_names

    # Convert tensor to DataFrame
    df_out = pd.DataFrame(X_features_tensor.cpu().numpy(), columns=all_feature_names)
    
    if dataset_label:
        print(f"{dataset_label}: Feature dataframe shape = {df_out.shape}")
    
    return df_out


import os
import joblib
import json

def save_models_and_params(super_learner, base_models, label, save_params=True, output_dir="intermediate"):
    """
    Save the super learner model, individual base models, and optionally best parameters.

    Parameters:
        super_learner: fitted StackingClassifier
        base_models: dict of fitted RandomizedSearchCV models (e.g., {'lasso': ..., 'rf': ..., 'xgb': ...})
        label: str, used to name files (e.g., "Mobility 65–75")
        save_params: bool, whether to save base_models' best_params_
        output_dir: str, folder to save models and parameters

    Returns:
        None
    """
    os.makedirs(output_dir, exist_ok=True)
    clean_label = label.lower().replace(" ", "_").replace("–", "-")

    # Save super learner model
    super_path = os.path.join(output_dir, f"{clean_label}_super_learner.pkl")
    joblib.dump(super_learner, super_path)
    print(f"✅ Saved Super Learner to: {super_path}")

    # Save base models
    for name, model in base_models.items():
        model_path = os.path.join(output_dir, f"{clean_label}_{name}_model.pkl")
        joblib.dump(model, model_path)
        print(f"✅ Saved Base Model ({name}) to: {model_path}")

    # Optionally save best parameters
    if save_params:
        best_params_dict = {name: model.best_params_ for name, model in base_models.items()}
        json_path = os.path.join(output_dir, f"{clean_label}_best_params.json")
        with open(json_path, "w") as f:
            json.dump(best_params_dict, f, indent=2)
        print(f"✅ Saved Best Parameters to: {json_path}")



def refit_and_save_final_models(X, y, base_models, label, output_dir="intermediate", save=True):
    import os, joblib
    from sklearn.linear_model import LogisticRegression
    from sklearn.ensemble import RandomForestClassifier
    from xgboost import XGBClassifier
    from sklearn.pipeline import make_pipeline
    from sklearn.preprocessing import StandardScaler

    if save:
        os.makedirs(output_dir, exist_ok=True)
    clean_label = label.lower().replace(" ", "_").replace("–", "-")

    final_models = {}
    y_np = y.squeeze().numpy()
    ratio = float((y_np == 0).sum()) / float((y_np == 1).sum())

    for name, model in base_models.items():
        print(f"\n🔁 Refitting {name.upper()} on full data...")

        best_params = model.best_params_

        if name == "lasso":
            final_model = make_pipeline(
                StandardScaler(),
                LogisticRegression(
                    penalty='l1', solver='saga', class_weight='balanced',
                    max_iter=1000, random_state=42,
                    **{k.split("__")[1]: v for k, v in best_params.items()}
                )
            )
        elif name == "rf":
            final_model = RandomForestClassifier(
                class_weight='balanced', random_state=42, **best_params
            )
        elif name == "xgb":
            final_model = XGBClassifier(
                use_label_encoder=False, eval_metric='logloss',
                scale_pos_weight=ratio, random_state=42, **best_params
            )
        else:
            print(f"⚠️ Skipping unknown model: {name}")
            continue

        final_model.fit(X, y)
        final_models[name] = final_model

        if save:
            path = os.path.join(output_dir, f"{clean_label}_{name}_final_model.pkl")
            joblib.dump(final_model, path)
            print(f"✅ Saved {name.upper()} refitted model to: {path}")

    return final_models


def refit_and_save_super_learner(X, y, base_models, label, output_dir="intermediate", save=True):
    import os, joblib
    from sklearn.linear_model import LogisticRegression
    from sklearn.ensemble import StackingClassifier

    if save:
        os.makedirs(output_dir, exist_ok=True)
    clean_label = label.lower().replace(" ", "_").replace("–", "-")

    estimators = [(name, model) for name, model in base_models.items()]

    final_est = LogisticRegression(
        penalty="l2",
        solver="liblinear",  # or "saga" if high-dim
        max_iter=1000,
        random_state=42
    )

    super_learner = StackingClassifier(
        estimators=estimators,
        final_estimator=final_est,
        passthrough=True,
        cv=5
    )

    print(f"\n🔁 Fitting SUPER LEARNER for {label}...")
    super_learner.fit(X, y)

    if save:
        path = os.path.join(output_dir, f"{clean_label}_super_learner.pkl")
        joblib.dump(super_learner, path)
        print(f"✅ Saved SUPER LEARNER to: {path}")

    return super_learner


import torch
import torch.nn.functional as F

def extract_treatment_features_from_medoid(X, feature_cols):
    """
    Extract trajectory-based features from longitudinal data X of shape [N, T, D].

    Features include:
    - Cumulative exposure to full-time and part-time employment
    - Cohabitation type summaries and transitions
    - Timing and duration of states
    - Volatility (entropy) and most recent status
    - Covariates: all 'pre_' and 'dt_n_years' variables, and welfare regime dummies

    Args:
        X (torch.Tensor): Long format tensor of shape [N, T, D]
        feature_cols (list of str): List of column names corresponding to X's last dimension

    Returns:
        torch.Tensor: Feature matrix of shape [N, F] for downstream use
    """
    N, T, D = X.shape
    fn_idx = {name: i for i, name in enumerate(feature_cols)}

    # --- Treatment variable groups
    work_vars = [
        "trt_full_time_employment",
        "trt_part_time_employment",
        "trt_in_education",
        "trt_not_working"
    ]
    cohab_vars = [
        "trt_not_cohabit_0_children",
        "trt_cohabit_0_children",
        "trt_not_cohabit_with_children",
        "trt_cohabit_with_children"
    ]

    work_idxs = [fn_idx[v] for v in work_vars]
    cohab_idxs = [fn_idx[v] for v in cohab_vars]

    work_treat = X[:, :, work_idxs]     # [N, T, 4]
    cohab_treat = X[:, :, cohab_idxs]   # [N, T, 4]

    # --- Cumulative and dominant exposure
    # Dominant work type
    cum_work_type = work_treat.sum(dim=1)             # [N, 4]
    dominant_work = torch.argmax(cum_work_type, dim=1)  # [N]
    dominant_work_oh = F.one_hot(dominant_work, num_classes=4).float()  # [N, 4]

    # Cumulative full-time and part-time 
    cum_work = work_treat[:, :, [0, 1]].sum(dim=1)  # [N, 2]

    # Dominant family type
    cum_cohab = cohab_treat.sum(dim=1)             # [N, 4]
    dominant_cohab = torch.argmax(cum_cohab, dim=1)  # [N]
    dominant_cohab_oh = F.one_hot(dominant_cohab, num_classes=4).float()  # [N, 4]

    # Cumulative years of cohabitation or having children 
    total_cohab_years = (cum_cohab[:, 1] + cum_cohab[:, 3]).unsqueeze(1)
    total_not_cohab_years = (cum_cohab[:, 0] + cum_cohab[:, 2]).unsqueeze(1)
    total_children_years = (cum_cohab[:, 2] + cum_cohab[:, 3]).unsqueeze(1)
    total_no_children_years = (cum_cohab[:, 0] + cum_cohab[:, 1]).unsqueeze(1)

    # --- Transition counts
    def count_transitions(onehot_tensor):
        labels = torch.argmax(onehot_tensor, dim=2)
        transitions = (labels[:, 1:] != labels[:, :-1]).sum(dim=1)
        return transitions.unsqueeze(1)

    trans_work = count_transitions(work_treat)
    trans_cohab = count_transitions(cohab_treat)

    # --- Timing of first events
    def first_occurrence(onehot_tensor, class_idx):
        labels = torch.argmax(onehot_tensor, dim=2)
        first = (labels == class_idx).float()
        idxs = torch.arange(T, device=X.device).view(1, -1)
        first_time = (first * idxs).masked_fill(first == 0, T).min(dim=1).values
        return first_time.unsqueeze(1)

    first_fulltime = first_occurrence(work_treat, class_idx=0)
    first_cohabit = first_occurrence(cohab_treat, class_idx=1)

    # --- Max spell duration
    def max_duration(onehot_tensor):
        labels = torch.argmax(onehot_tensor, dim=2)
        max_durs = []
        for row in labels:
            diffs = torch.cat([torch.tensor([1], device=row.device), (row[1:] != row[:-1]).int()])
            spell_ids = torch.cumsum(diffs, dim=0)
            _, counts = torch.unique(spell_ids, return_counts=True)
            max_durs.append(counts.max())
        return torch.stack(max_durs).unsqueeze(1)

    longest_work_spell = max_duration(work_treat)
    longest_cohab_spell = max_duration(cohab_treat)

    # --- Most recent status (one-hot)
    last_work_label = torch.argmax(work_treat[:, -1, :], dim=1)        # [N]
    last_cohab_label = torch.argmax(cohab_treat[:, -1, :], dim=1)      # [N]
    last_work_oh = F.one_hot(last_work_label, num_classes=4).float()   # [N, 4]
    last_cohab_oh = F.one_hot(last_cohab_label, num_classes=4).float() # [N, 4]

    # --- Entropy of trajectory
    def normalized_entropy(onehot_tensor):
        labels = torch.argmax(onehot_tensor, dim=2)
        entropies = []
        for row in labels:
            counts = torch.bincount(row, minlength=onehot_tensor.size(2)).float()
            probs = counts / counts.sum()
            log_probs = torch.log2(probs + 1e-8)
            entropy = -(probs * log_probs).sum()
            entropies.append(entropy / torch.log2(torch.tensor(onehot_tensor.size(2), dtype=torch.float)))
        return torch.stack(entropies).unsqueeze(1)

    entropy_work = normalized_entropy(work_treat)
    entropy_cohab = normalized_entropy(cohab_treat)
 

    # --- Final feature matrix
    return torch.cat([
        cum_work,               # [N, 2]
        total_cohab_years,      # [N, 1]
        total_not_cohab_years,  # [N, 1]
        total_children_years,   # [N, 1]
        total_no_children_years,# [N, 1]
        dominant_work_oh,  # [N, 4] one-hot
        dominant_cohab_oh,      # [N, 4] one-hot
        trans_work,             # [N, 1]
        trans_cohab,            # [N, 1]
        first_fulltime,         # [N, 1]
        first_cohabit,          # [N, 1]
        longest_work_spell,     # [N, 1]
        longest_cohab_spell,    # [N, 1]
        last_work_oh,           # [N, 4] one-hot
        last_cohab_oh,          # [N, 4] one-hot
        entropy_work,           # [N, 1]
        entropy_cohab           # [N, 1] 
    ], dim=1)


def generate_updated_list(base_tensor, medoid_features, cols_to_replace=30):
    updated_list = []
    for m in medoid_features:
        X_updated = base_tensor.clone()
        X_updated[:, :cols_to_replace] = m  # apply medoid's features to all rows
        updated_list.append(X_updated)
    return updated_list



import warnings
import numpy as np
import pandas as pd


################
## Function 1 ##
################

def counterfactual_y_under_X_medoid_features(
    tv_cov_model, 
    y_model,
    y_dic, 
    X_features_disease_65_75_dic_updated_list,
    X_features_outcome_65_75_dic_updated_list, 
    tv_cov_name="dt_n_years_disease_dic",
    outcome_feature_names=None,
    results_df=None,
): 
    """
    For each medoid, simulate counterfactual predictions by updating
    the disease features, predicting the covariate, and updating the
    outcome features accordingly before predicting the outcome.
    """

    counterfactuals_y = {}
    counterfactual_X = {}
    ates = {}
    stand_ates = {}
    risk_ratios = {}
    atts = {}

    n_medoids = len(X_features_disease_65_75_dic_updated_list)
    results_df['y_obs'] = y_dic # store for ATT calculation

    cluster_medoid_map = {
        'Cluster 1: FT w Child': 'medoid_0', 
        'Cluster 2: FT w/o Child': 'medoid_1', 
        'Cluster 3: UE w Child': 'medoid_2',
        'Cluster 4: PT w Child': 'medoid_3'
    }

    for medoid_id in range(n_medoids):
        print(f"\n--- Medoid {medoid_id} ---")

        # Get updated covariate features for this medoid
        X_arth = X_features_disease_65_75_dic_updated_list[medoid_id]
        X_mob = X_features_outcome_65_75_dic_updated_list[medoid_id]

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")

            # Predict time-varying covariate for all units
            tv_cov_pred = tv_cov_model.predict(X_arth)
            print(f"  Predicted {tv_cov_name}: mean={np.mean(tv_cov_pred):.3f}")

            # Update mobility feature matrix
            X_mob_df = pd.DataFrame(X_mob.numpy(), columns=outcome_feature_names)
            X_mob_df[tv_cov_name] = tv_cov_pred

            # Predict counterfactual outcome
            y_cf = y_model.predict_proba(X_mob_df)[:, 1]
            mean_y_cf = np.mean(y_cf)

        print(f" Potential outcome: mean={mean_y_cf:.3f}")

        # Store results
        counterfactuals_y[f"medoid_{medoid_id}"] = y_cf
        counterfactual_X[f"medoid_{medoid_id}"] = X_mob_df
        ates[f"medoid_{medoid_id}"] = (mean_y_cf - y_dic.mean()) 
        stand_ates[f"medoid_{medoid_id}"] = (mean_y_cf - y_dic.mean()) / y_dic.mean()
        risk_ratios[f"medoid_{medoid_id}"] = (mean_y_cf) / y_dic.mean()

        # Calculate ATT
        results_df[f"medoid_{medoid_id}_y_cf"] = y_cf
        results_df_summ_temp = pd.DataFrame()
        results_df_summ_temp = results_df[['medoid', 'y_obs', f"medoid_{medoid_id}_y_cf"]]\
            .groupby(['medoid'])\
            .agg({
                f"medoid_{medoid_id}_y_cf": 'mean',
                'y_obs': 'mean'
            })\
            .assign(att = lambda d: d[f"medoid_{medoid_id}_y_cf"] - d['y_obs'])\
            .reset_index()
        atts[f"medoid_{medoid_id}"] = results_df_summ_temp[results_df_summ_temp['medoid'] == f"medoid_{medoid_id}"]['att'].iloc[0]

        print(f" Relative average treatment effects: {((mean_y_cf - y_dic.mean()) / y_dic.mean()):.3f}: Negative sign meaning less risk of health problem")


    return counterfactuals_y, counterfactual_X, ates, stand_ates, risk_ratios, atts


def flatten_ates_dict(ates_dict, prefix):
    """Flatten medoid ATE dictionary and convert tensors to floats."""
    return {
        f"{prefix}_medoid_{k.split('_')[-1]}": v.item() if hasattr(v, 'item') else float(v)
        for k, v in ates_dict.items()
    }

from pathlib import Path

def save_results_df(df, label, folder="Results", prefix="ate", suffix="all", ext="csv"):
    # Ensure folder exists
    Path(folder).mkdir(parents=True, exist_ok=True)

    # Sanitize label (optional: lowercase, replace spaces, remove special chars)
    safe_label = label.lower().replace(" ", "_").replace("–", "-")

    # Build full file path
    filename = f"{prefix}_{safe_label}_{suffix}.{ext}"
    path = Path(folder) / filename

    # Save DataFrame
    df.to_csv(path, index=False)
    print(f"Saved: {path}")



import pandas as pd

def summarize_bootstrap_percentile_ci(df, drop_seed=True):
    """
    Summarize bootstrap results with percentile-based confidence intervals
    and significance stars:
      *    p ≤ 0.05
      **   p ≤ 0.01
      ***  p ≤ 0.001

    Parameters:
    - df (pd.DataFrame): Bootstrap results with columns for each target.
    - drop_seed (bool): Whether to drop the 'seed' column.

    Returns:
    - pd.DataFrame: Summary with mean, std, ci_lower, ci_upper, stars.
    """
    if drop_seed and "seed" in df.columns:
        df = df.drop(columns=["seed"])

    # Get quantile-based summary
    summary = df.agg(
        ["mean", "std", lambda x: x.quantile(0.025), lambda x: x.quantile(0.975)]
    ).T.reset_index()
    summary.columns = ["medoid_regime", "mean", "std", "ci_lower", "ci_upper"]

    # Compute p-value by proportion of bootstraps crossing zero
    def estimate_p(x):
        below = (x < 0).mean()
        above = (x > 0).mean()
        return 2 * min(below, above)

    p_values = df.apply(estimate_p)
    summary["p_value"] = p_values.values

    # Add significance stars
    def stars(p):
        if p <= 0.001:
            return "***"
        elif p <= 0.01:
            return "**"
        elif p <= 0.05:
            return "*"
        else:
            return ""

    summary["stars"] = summary["p_value"].apply(stars)
    return summary
