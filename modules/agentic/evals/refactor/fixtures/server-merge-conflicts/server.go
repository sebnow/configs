package api

import (
	"context"
	"errors"

	"github.com/google/uuid"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	sdk "example.com/moderation/sdk/go"
)

// Server is the gRPC API surface for the moderation service. Every handler
// the service exposes lives on this receiver and is implemented in this file.
//
// History note from the team's git log:
//
//	"api/server.go is getting too big and is contentious — every feature branch
//	 touches it, we resolved conflicts here in four of the last ten merges."
type Server struct {
	logger          Logger
	reports         ReportRepository
	decisions       DecisionRepository
	rulesets        RulesetRepository
	rules           RuleRepository
	queueItems      QueueItemRepository
	queues          QueueRepository
	issueCategories IssueCategoryRepository
	apiKeys         APIKeyRepository
	clients         ClientRepository
}

// ---- Reports -------------------------------------------------------------

func (s *Server) FindReports(ctx context.Context, req *sdk.FindReportsRequest) (*sdk.FindReportsResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	ids, hasMore, err := s.reports.FindReports(ctx, tenantID, req.GetCursor())
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to find reports")
	}
	reports, err := s.reports.GetReports(ctx, tenantID, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load reports")
	}
	return &sdk.FindReportsResponse{Reports: toReportProtos(reports), HasMore: hasMore}, nil
}

func (s *Server) BatchGetReport(ctx context.Context, req *sdk.BatchGetReportRequest) (*sdk.BatchGetReportResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	ids, err := parseUUIDs(req.GetReportIds())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "report_ids contains invalid id")
	}
	reports, err := s.reports.GetReports(ctx, tenantID, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load reports")
	}
	return &sdk.BatchGetReportResponse{Reports: toReportProtos(reports)}, nil
}

// ---- Decisions -----------------------------------------------------------

func (s *Server) FindDecisions(ctx context.Context, req *sdk.FindDecisionsRequest) (*sdk.FindDecisionsResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	ids, hasMore, err := s.decisions.FindDecisions(ctx, tenantID, req.GetCursor())
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to find decisions")
	}
	decisions, err := s.decisions.GetDecisions(ctx, tenantID, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load decisions")
	}
	return &sdk.FindDecisionsResponse{Decisions: toDecisionProtos(decisions), HasMore: hasMore}, nil
}

func (s *Server) BatchGetDecision(ctx context.Context, req *sdk.BatchGetDecisionRequest) (*sdk.BatchGetDecisionResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	ids, err := parseUUIDs(req.GetDecisionIds())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "decision_ids contains invalid id")
	}
	decisions, err := s.decisions.GetDecisions(ctx, tenantID, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load decisions")
	}
	return &sdk.BatchGetDecisionResponse{Decisions: toDecisionProtos(decisions)}, nil
}

// ---- Rulesets ------------------------------------------------------------

func (s *Server) FindRulesets(ctx context.Context, req *sdk.FindRulesetsRequest) (*sdk.FindRulesetsResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	ids, err := s.rulesets.FindRulesets(ctx, tenantID)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to find rulesets")
	}
	rulesets, err := s.rulesets.GetRulesets(ctx, tenantID, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load rulesets")
	}
	return &sdk.FindRulesetsResponse{Rulesets: toRulesetProtos(rulesets)}, nil
}

func (s *Server) AddRuleset(ctx context.Context, req *sdk.AddRulesetRequest) (*sdk.AddRulesetResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	id, err := s.rulesets.AddRuleset(ctx, tenantID, req.GetRuleset())
	if err != nil {
		if errors.Is(err, ErrRulesetAlreadyExists) {
			return nil, status.Error(codes.AlreadyExists, "ruleset already exists")
		}
		return nil, status.Error(codes.Internal, "failed to add ruleset")
	}
	return &sdk.AddRulesetResponse{Id: id.String()}, nil
}

// ---- Rules ---------------------------------------------------------------

func (s *Server) FindRules(ctx context.Context, req *sdk.FindRulesRequest) (*sdk.FindRulesResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	ids, err := s.rules.FindRules(ctx, tenantID, req.GetRulesetId())
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to find rules")
	}
	rules, err := s.rules.GetRules(ctx, tenantID, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load rules")
	}
	return &sdk.FindRulesResponse{Rules: toRuleProtos(rules)}, nil
}

func (s *Server) AddRule(ctx context.Context, req *sdk.AddRuleRequest) (*sdk.AddRuleResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	rulesetID, err := uuid.Parse(req.GetRulesetId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "ruleset_id is not valid")
	}
	id, err := s.rules.AddRule(ctx, tenantID, rulesetID, req.GetRule())
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to add rule")
	}
	return &sdk.AddRuleResponse{Id: id.String()}, nil
}

// ---- Queue items ---------------------------------------------------------

func (s *Server) FindQueueItems(ctx context.Context, req *sdk.FindQueueItemsRequest) (*sdk.FindQueueItemsResponse, error) {
	tenantID, err := uuid.Parse(req.GetTenantId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "tenant_id is not valid")
	}
	queueID, err := uuid.Parse(req.GetQueueId())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "queue_id is not valid")
	}
	ids, hasMore, err := s.queueItems.Find(ctx, tenantID, queueID, req.GetCursor())
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to find queue items")
	}
	items, err := s.queueItems.GetQueueItems(ctx, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load queue items")
	}
	return &sdk.FindQueueItemsResponse{Items: toQueueItemProtos(items), HasMore: hasMore}, nil
}

func (s *Server) BatchGetQueueItem(ctx context.Context, req *sdk.BatchGetQueueItemRequest) (*sdk.BatchGetQueueItemResponse, error) {
	ids, err := parseUUIDs(req.GetQueueItemIds())
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "queue_item_ids contains invalid id")
	}
	items, err := s.queueItems.GetQueueItems(ctx, ids)
	if err != nil {
		return nil, status.Error(codes.Internal, "failed to load queue items")
	}
	return &sdk.BatchGetQueueItemResponse{Items: toQueueItemProtos(items)}, nil
}
