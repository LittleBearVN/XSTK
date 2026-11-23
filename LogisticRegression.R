##############################################################################
# BAI TAP LON XAC SUAT THONG KE - INTERNET ADVERTISEMENTS (ad / nonad)
#
# CACH CHAY
#   1. Dat file nay va add.csv trong cung mot thu muc, mo thu muc do
#      (RStudio: Session > Set Working Directory > To Source File Location)
#   2. Bam bieu tuong cay choi o tab Environment de xoa bien cu
#   3. Chay tung dong bang Ctrl + Enter, hoac chay ca file bang nut Source
#
# CAU TRUC
#   PHAN 1: Tien xu ly du lieu            -> Muc 3 cua report
#   PHAN 2: Thong ke mo ta (EDA)          -> Muc 4
#   PHAN 3: Hoi quy logistic              -> Muc 5
#   PHAN 4: Random Forest va so sanh      -> Muc 6
##############################################################################


# ----------------------------------------------------------------------------
# CAI VA GOI CAC GOI CAN DUNG
# ----------------------------------------------------------------------------
goi <- c("corrplot", "pROC", "MASS", "randomForest", "caret", "e1071", "car")
for (g in goi) {
  if (!require(g, character.only = TRUE)) {
    install.packages(g, dependencies = TRUE)
    library(g, character.only = TRUE)
  }
}


##############################################################################
# PHAN 1. TIEN XU LY DU LIEU
##############################################################################

# Doc du lieu (cac o thieu trong file duoc ghi bang dau ?)
add_goc <- read.csv("add.csv", header = TRUE,
                    na.strings = c("?", " ?", "  ?", "NA", ""),
                    strip.white = TRUE)

# Bo cot dau tien (so thu tu)
add_goc <- add_goc[, -1]

# Kiem tra dong trung nhau tren toan bo cac cot goc (truoc khi bo cot nao)
cat("So dong trung nhau (toan bo cot goc):", sum(duplicated(add_goc)), "\n")

# Tao ma nhom tu TOAN BO 1558 cot dac trung goc (khong bao gom nhan Target)
# De gom cac ban ghi trung het dac trung vao cung mot tap khi chia train/test,
# dong thoi van giu cac banner khac trang web co cung kich thuoc chuan o ca hai tap.
dac_trung_goc <- add_goc[, -ncol(add_goc), drop = FALSE]
chuoi_goc     <- do.call(paste, c(dac_trung_goc, sep = "|"))
nhom_goc      <- match(chuoi_goc, unique(chuoi_goc))

# Chi giu: height, width, ratio (cot 1-3 sau khi bo STT) va nhan ad (cot cuoi).
# Cac cot dac trung URL-term bi bo khoi mo hinh theo huong dan de bai.
add_full <- data.frame(
  Height   = suppressWarnings(as.numeric(as.character(add_goc[, 1]))),
  Width    = suppressWarnings(as.numeric(as.character(add_goc[, 2]))),
  Ratio    = suppressWarnings(as.numeric(as.character(add_goc[, 3]))),
  Target   = trimws(as.character(add_goc[, ncol(add_goc)])),
  Nhom_goc = nhom_goc,
  stringsAsFactors = FALSE
)

# Xem gia tri nhan thuc te truoc khi ma hoa: ad = 1, nonad = 0
cat("Cac gia tri nhan thuc te truoc khi ma hoa:\n")
print(table(add_full$Target))
add_full$Target <- ifelse(add_full$Target == "ad.", 1, 0)

cat("So dong ban dau:", nrow(add_full), "\n")
cat("So o thieu (NA) moi cot:\n")
print(colSums(is.na(add_full[, c("Height", "Width", "Ratio", "Target")])))
cat("Ty le % NA moi cot:\n")
print(round(colMeans(is.na(add_full[, c("Height", "Width", "Ratio", "Target")])) * 100, 1))

# Histogram cua Height va Width TRUOC khi xu ly NA
par(mfrow = c(1, 2))
hist(add_full$Height, main = "Histogram of Height with NAs", xlab = "Height", col = "skyblue", border = "black")
hist(add_full$Width,  main = "Histogram of Width with NAs",  xlab = "Width",  col = "salmon",  border = "black")
par(mfrow = c(1, 1))

cat("Ty le ad / nonad ban dau (%):\n")
print(round(prop.table(table(add_full$Target)) * 100, 2))

# Bo du lieu du 3 bien (complete cases) phuc vu thong ke mo ta (EDA)
add <- na.omit(add_full[, c("Height", "Width", "Ratio", "Target")])

cat("So dong du 3 bien dung cho EDA:", nrow(add), "\n")
cat("So dong trung nhau trong du lieu sach:", sum(duplicated(add)), "\n")
cat("Ty le ad / nonad sau khi loc NA cho EDA (%):\n")
print(round(prop.table(table(add$Target)) * 100, 2))

# Luu du lieu sach (de ca nhom dung chung)
write.csv(add, "add_clean.csv", row.names = FALSE)


##############################################################################
# PHAN 2. THONG KE MO TA (EDA)
##############################################################################

# Ham tinh cac thong ke mau
thong_ke <- function(x) {
  c(n = length(x), mean = mean(x), sd = sd(x), min = min(x),
    Q1 = unname(quantile(x, 0.25)), median = median(x),
    Q3 = unname(quantile(x, 0.75)), max = max(x))
}

# Thong ke cua 3 bien lien tuc theo 2 nhom (cot "0" = nonad, cot "1" = ad)
for (bien in c("Height", "Width", "Ratio")) {
  cat("\nBien:", bien, "\n")
  print(round(sapply(split(add[[bien]], add$Target), thong_ke), 3))
}

# Bang tan so va ty le % cua bien ad
cat("\nTan so cua ad (0 = nonad, 1 = ad):\n")
print(table(add$Target))
print(round(prop.table(table(add$Target)) * 100, 1))

# --- Do thi ---
# Histogram cua 3 bien lien tuc
par(mfrow = c(1, 3))
hist(add$Height, main = "Histogram of Height", xlab = "Height", col = "skyblue")
hist(add$Width,  main = "Histogram of Width",  xlab = "Width",  col = "salmon")
hist(add$Ratio,  main = "Histogram of Ratio",  xlab = "Ratio",  col = "lightgreen")
par(mfrow = c(1, 1))

# Barplot cua bien ad
barplot(table(add$Target), names.arg = c("Non-Ad", "Ad"),
        main = "Barplot of Target", ylab = "Frequency", col = c("gray", "orange"))

# Boxplot cua tung bien theo nhom ad / nonad
par(mfrow = c(1, 3))
boxplot(Height ~ Target, data = add, names = c("Non-Ad", "Ad"), main = "Height by Target", col = "lightblue")
boxplot(Width ~ Target,  data = add, names = c("Non-Ad", "Ad"), main = "Width by Target",  col = "lightgreen")
boxplot(Ratio ~ Target,  data = add, names = c("Non-Ad", "Ad"), main = "Ratio by Target",  col = "lightpink")
par(mfrow = c(1, 1))

# Do thi phan tan: Target theo tung bien, kem duong cong logistic (mau do)
mh_h <- glm(Target ~ Height, data = add, family = binomial)
mh_w <- glm(Target ~ Width,  data = add, family = binomial)
mh_r <- glm(Target ~ Ratio,  data = add, family = binomial)
par(mfrow = c(1, 3))
plot(add$Height, add$Target, xlab = "Height", ylab = "Target", main = "Logistic Regression: Target vs Height",
     pch = 16, col = rgb(0, 0, 1, 0.3))
curve(predict(mh_h, newdata = data.frame(Height = x), type = "response"), add = TRUE, col = "red", lwd = 2)
plot(add$Width, add$Target, xlab = "Width", ylab = "Target", main = "Logistic Regression: Target vs Width",
     pch = 16, col = rgb(0, 0.5, 0, 0.3))
curve(predict(mh_w, newdata = data.frame(Width = x), type = "response"), add = TRUE, col = "red", lwd = 2)
plot(add$Ratio, add$Target, xlab = "Ratio", ylab = "Target", main = "Logistic Regression: Target vs Ratio",
     pch = 16, col = rgb(0.7, 0, 0, 0.3))
curve(predict(mh_r, newdata = data.frame(Ratio = x), type = "response"), add = TRUE, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# Ma tran tuong quan
tuong_quan <- cor(add[, c("Height", "Width", "Ratio")])
print(round(tuong_quan, 3))
corrplot(tuong_quan, method = "number", title = "Correlation matrix", mar = c(0, 0, 1, 0))


##############################################################################
# PHAN 3. THONG KE SUY DIEN - HOI QUY LOGISTIC
##############################################################################

# --- 3.1 Chia du lieu: gom cac dong cung bo dac trung goc vao cung mot tap, chia 70/30 phan tang ---
add_full$Target_f <- factor(add_full$Target, levels = c(0, 1), labels = c("nonad", "ad"))

# Ham chia phan tang theo nhom bo dac trung goc (toan bo 1558 dac trung)
# Dam bao cac dong trung nhau nam chung o train hoac test (0% ro ri du lieu)
chia_nhom_phan_tang <- function(df, p = 0.7) {
  ds_nhom   <- unique(df$Nhom_goc)
  tang_nhom <- sapply(ds_nhom, function(k) {
    idx_k <- which(df$Nhom_goc == k)
    nhan  <- ifelse(mean(df$Target[idx_k]) >= 0.5, "ad", "nonad")
    co    <- ifelse(length(idx_k) >= 3, "lon", "nho")
    paste(nhan, co, sep = "_")
  })
  tang_nhom  <- factor(tang_nhom)
  idx_nhom   <- createDataPartition(tang_nhom, p = p, list = FALSE)
  nhom_train <- ds_nhom[idx_nhom]
  return(df$Nhom_goc %in% nhom_train)
}

# Ham dien khuyet bang trung vi CHI TU TAP TRAIN & bien doi logarit (theo Muc 1.4 & 3.7)
# Giu nguyen co mau 3279 dong, khong ro ri thong tin tu tap test, khac phuc phi tuyen.
tien_xu_ly_va_dac_trung <- function(tr, te) {
  med_h <- median(tr$Height, na.rm = TRUE)
  med_w <- median(tr$Width,  na.rm = TRUE)
  med_r <- median(tr$Ratio,  na.rm = TRUE)
  
  tr$Height[is.na(tr$Height)] <- med_h
  tr$Width[is.na(tr$Width)]   <- med_w
  tr$Ratio[is.na(tr$Ratio)]   <- med_r
  
  te$Height[is.na(te$Height)] <- med_h
  te$Width[is.na(te$Width)]   <- med_w
  te$Ratio[is.na(te$Ratio)]   <- med_r
  
  mu_lh <- mean(log(pmax(tr$Height, 1e-6)))
  mu_lw <- mean(log(pmax(tr$Width,  1e-6)))
  mu_lr <- mean(log(pmax(tr$Ratio,  1e-6)))
  
  tr$lh   <- log(pmax(tr$Height, 1e-6)) - mu_lh
  tr$lw   <- log(pmax(tr$Width,  1e-6)) - mu_lw
  tr$lr   <- log(pmax(tr$Ratio,  1e-6)) - mu_lr
  tr$lh2  <- tr$lh^2
  tr$lr2  <- tr$lr^2
  tr$Area <- tr$Height * tr$Width
  
  te$lh   <- log(pmax(te$Height, 1e-6)) - mu_lh
  te$lw   <- log(pmax(te$Width,  1e-6)) - mu_lw
  te$lr   <- log(pmax(te$Ratio,  1e-6)) - mu_lr
  te$lh2  <- te$lh^2
  te$lr2  <- te$lr^2
  te$Area <- te$Height * te$Width
  
  list(train = tr, test = te)
}

set.seed(123)
la_train <- chia_nhom_phan_tang(add_full, p = 0.7)
dt       <- tien_xu_ly_va_dac_trung(add_full[la_train, ], add_full[!la_train, ])
train    <- dt$train
test     <- dt$test

# Kiem tra so dong test trung bo dac trung goc voi train (phai bang 0)
so_trung <- sum(test$Nhom_goc %in% train$Nhom_goc)

cat("\nTrain:", nrow(train), "dong | Test:", nrow(test), "dong\n")
cat("So dong test trung bo dac trung goc voi train:", so_trung, "/", nrow(test),
    paste0("(", round(so_trung / nrow(test) * 100, 2), "%)\n"))
cat("Ty le ad trong train va test (%):\n")
print(round(c(train = mean(train$Target), test = mean(test$Target)) * 100, 2))

# --- 3.2 Xay dung mo hinh logistic (so sanh mo hinh tuyen tinh goc & mo hinh bien doi log) ---
mo_hinh_goc <- glm(Target ~ Height + Width + Ratio, data = train, family = binomial)
cat("\n--- Mo hinh logistic tuyen tinh goc (truoc bien doi log) ---\n")
print(summary(mo_hinh_goc))

# Mo hinh logistic sau bien doi logarit quy tam (khac phuc phi tuyen theo Muc 3.7 huong dan)
mo_hinh <- glm(Target ~ lh + lw + lh2 + lr2, data = train, family = binomial)
cat("\n--- Mo hinh logistic day du sau bien doi log (lh, lw, lh2, lr2) ---\n")
print(summary(mo_hinh))

# --- 3.3 Toi uu mo hinh bang AIC (stepAIC) ---
mo_hinh_tot <- stepAIC(mo_hinh, direction = "both", trace = FALSE)
print(summary(mo_hinh_tot))
cat("AIC mo hinh tuyen tinh goc:", AIC(mo_hinh_goc),
    "| AIC mo hinh log day du:", AIC(mo_hinh),
    "| AIC sau stepAIC:", AIC(mo_hinh_tot), "\n")

# Odds ratio va khoang tin cay 95%: cho biet muc do anh huong cua tung bien
bang_or <- exp(cbind(OR = coef(mo_hinh_tot), confint.default(mo_hinh_tot)))
print(round(bang_or, 4))

# --- 3.4 Du bao tren tap test ---
# Ham tinh accuracy, precision, recall, F1, AUC
danh_gia <- function(p, thuc_te, nguong = 0.5) {
  du_doan <- ifelse(p > nguong, 1, 0)
  TP <- sum(du_doan == 1 & thuc_te == 1)
  TN <- sum(du_doan == 0 & thuc_te == 0)
  FP <- sum(du_doan == 1 & thuc_te == 0)
  FN <- sum(du_doan == 0 & thuc_te == 1)
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1        <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  c(accuracy  = (TP + TN) / length(thuc_te),
    precision = precision,
    recall    = recall,
    F1        = f1,
    AUC       = as.numeric(auc(roc(thuc_te, p, quiet = TRUE))))
}

p_lr <- predict(mo_hinh_tot, newdata = test, type = "response")

# Nguong 0.5 (mac dinh theo huong dan)
cat("\nMa tran nham lan (nguong 0.5):\n")
print(table(Thuc_te = test$Target, Du_doan = ifelse(p_lr > 0.5, 1, 0)))
ket_qua_lr_05 <- danh_gia(p_lr, test$Target, 0.5)

# So sanh voi mo hinh luon doan nonad (de thay accuracy don le co the danh lua)
cat("Accuracy neu luon doan nonad:", round(mean(test$Target == 0), 4), "\n")

# --- 3.5 Duong cong ROC va AUC ---
roc_lr <- roc(test$Target, p_lr, quiet = TRUE)
plot(roc_lr, main = "ROC curve - Logistic regression")
cat("AUC logistic:", round(as.numeric(auc(roc_lr)), 4), "\n")

# --- 3.6 Kiem tra gia dinh cua mo hinh ---

# (a) Da cong tuyen: VIF. VIF > 5 (hoac 10) la dang lo
cat("\nVIF:\n")
if (length(coef(mo_hinh_tot)) > 2) {
  print(round(vif(mo_hinh_tot), 3))
} else {
  cat("Mo hinh toi uu chi con 1 bien doc lap nen khong co da cong tuyen.\n")
}

# (b) Ngoai lai va diem anh huong manh: khoang cach Cook (nguong 4/n) va phan du chuan hoa
cook <- cooks.distance(mo_hinh_tot)
plot(cook, type = "h", main = "Cook's distance", ylab = "Cook's distance")
abline(h = 4 / nrow(train), col = "red", lty = 2)
cat("\nSo diem co Cook > 4/n:", sum(cook > 4 / nrow(train)), "\n")
cat("Cook lon nhat:", round(max(cook), 4), "\n")
phan_du <- rstandard(mo_hinh_tot)
cat("So diem co |phan du chuan hoa| > 3:", sum(abs(phan_du) > 3), "\n")

# (c) Quan he tuyen tinh giua logit(P) va bien: kiem dinh Box-Tidwell tren 3 bien goc
# Them cac so hang x * log(x). Neu p-value < 0.05 thi gia dinh tuyen tinh goc bi vi pham
bt_data <- train[train$Height > 0 & train$Width > 0 & train$Ratio > 0, ]
bt <- glm(Target ~ Height + Width + Ratio +
            I(Height * log(Height)) + I(Width * log(Width)) + I(Ratio * log(Ratio)),
          data = bt_data, family = binomial)
cat("\nBox-Tidwell tren 3 bien goc (xem 3 dong cuoi - ly do can bien doi log):\n")
print(round(summary(bt)$coefficients, 4))


##############################################################################
# PHAN 4. THAO LUAN VA MO RONG - RANDOM FOREST
##############################################################################

# --- 4.1 Xay dung Random Forest tren CUNG tap train / test ---
set.seed(123)
rf <- randomForest(Target_f ~ Height + Width + Ratio + Area, data = train,
                   ntree = 500, mtry = 2, nodesize = 5, importance = TRUE)
print(rf)

p_rf <- predict(rf, newdata = test, type = "prob")[, "ad"]

cat("\nMa tran nham lan Random Forest (nguong 0.5):\n")
print(table(Thuc_te = test$Target, Du_doan = ifelse(p_rf > 0.5, 1, 0)))
ket_qua_rf <- danh_gia(p_rf, test$Target, 0.5)

# Do quan trong cua tung bien
print(round(importance(rf), 3))
varImpPlot(rf, main = "Random Forest - variable importance")

# --- 4.2 ROC cua hai mo hinh tren cung mot hinh ---
roc_rf <- roc(test$Target, p_rf, quiet = TRUE)
plot(roc_lr, col = "blue", lwd = 2, main = "ROC: Logistic vs Random Forest")
lines(roc_rf, col = "red", lwd = 2)
legend("bottomright", legend = c("Logistic", "Random Forest"),
       col = c("blue", "red"), lwd = 2)

# --- 4.3 Bang so sanh hai mo hinh tren tap test ---
so_sanh <- rbind("Logistic (nguong 0.5)"      = ket_qua_lr_05,
                 "Random Forest (nguong 0.5)" = ket_qua_rf)
cat("\nSo sanh tren tap test:\n")
print(round(so_sanh, 4))

# --- 4.4 Do on dinh: lap lai viec chia du lieu 30 lan (gom nhom dac trung goc trong moi lan) ---
# Moi lan chia khac nhau, ghi lai accuracy va AUC cua hai mo hinh (nguong 0.5)
so_lan <- 30
ket_qua_lap <- data.frame()
for (i in 1:so_lan) {
  set.seed(i)
  in_tr <- chia_nhom_phan_tang(add_full, p = 0.7)
  dt_i  <- tien_xu_ly_va_dac_trung(add_full[in_tr, ], add_full[!in_tr, ])
  tr    <- dt_i$train
  te    <- dt_i$test
  
  m_lr <- glm(formula(mo_hinh_tot), data = tr, family = binomial)
  d_lr <- danh_gia(predict(m_lr, newdata = te, type = "response"), te$Target, 0.5)
  
  m_rf <- randomForest(Target_f ~ Height + Width + Ratio + Area, data = tr,
                       ntree = 500, mtry = 2, nodesize = 5)
  d_rf <- danh_gia(predict(m_rf, newdata = te, type = "prob")[, "ad"], te$Target, 0.5)
  
  ket_qua_lap <- rbind(ket_qua_lap,
                       data.frame(Model = "Logistic",      accuracy = as.numeric(d_lr["accuracy"]), AUC = as.numeric(d_lr["AUC"])),
                       data.frame(Model = "Random Forest", accuracy = as.numeric(d_rf["accuracy"]), AUC = as.numeric(d_rf["AUC"])))
}

cat("\nTrung binh va do lech chuan qua", so_lan, "lan chia (do lech chuan nho = on dinh hon):\n")
print(aggregate(cbind(accuracy, AUC) ~ Model, data = ket_qua_lap,
                FUN = function(x) round(c(mean = mean(x), sd = sd(x)), 4)))

par(mfrow = c(1, 2))
boxplot(accuracy ~ Model, data = ket_qua_lap, main = "Accuracy over 30 splits", col = c("lightblue", "lightgreen"))
boxplot(AUC ~ Model, data = ket_qua_lap, main = "AUC over 30 splits", col = c("lightblue", "lightgreen"))
par(mfrow = c(1, 1))

cat("\nXong.\n")