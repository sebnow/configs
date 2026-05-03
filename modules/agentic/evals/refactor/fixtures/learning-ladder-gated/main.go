package main

import (
	"log"
	"net/http"
	"time"

	"example.com/app/report"
)

func handleReport(w http.ResponseWriter, r *http.Request) {
	userID := r.URL.Query().Get("user_id")
	from := time.Now().AddDate(0, -1, 0)
	to := time.Now()

	rep, err := report.FetchReport(userID, from, to)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	log.Printf("report for %s: %d rows", rep.UserID, len(rep.Rows))
	w.WriteHeader(http.StatusOK)
}

func main() {
	http.HandleFunc("/report", handleReport)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
