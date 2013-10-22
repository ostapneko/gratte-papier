gratteAppModule = angular.module('gratteApp', [])

gratteAppModule
    .controller('DocCtrl', ['$scope', '$http', ($scope, $http) ->
        $scope.searchQuery = ""

        $scope.search = ->
            opts =
                params:
                    q: $scope.searchQuery
            if $scope.searchQuery == ""
                $scope.docs = []
            else
                $http.get('/search', opts)
                .success( (data, status, headers, config) ->
                    $scope.docs = data["documents"]
                )
                .error( (data, status, headers, config) ->
                    console.log("Error #{status}: #{data}")
                )

        $scope.$watch('$scope.searchQuery', -> $scope.search)
        $scope.search()
    ])
    .filter('yearMonth', ->
        (input) -> "#{input.date.month || ''} #{input.date.year}"
    )
    .filter('shortText', ->
        (input) ->
            text = input.scannedText
            if text then text.slice(0,500) + "..." else ""
    )
    .filter('path', ->
        (input) -> "/docs#{input.path}"
    )
