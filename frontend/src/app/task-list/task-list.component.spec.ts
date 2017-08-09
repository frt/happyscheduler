import { DebugElement } from '@angular/core';
import { TestBed, async, inject } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpModule, Http } from '@angular/http';
import { InfoMessagesService } from '../info-messages.service';

import { Observable} from 'rxjs/Observable';
import 'rxjs/add/observable/of';

import { ComponentFixtureAutoDetect } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

// new-task stuff
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { NewTaskComponent } from '../new-task/new-task.component';

import { TaskListComponent } from './task-list.component';

describe('TaskListComponent', () => {
    let fixture;
    let component: TaskListComponent;
    let el;
    let initialNrOfTasks;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                ReactiveFormsModule,
                FormsModule,
                RouterTestingModule,
                HttpModule
            ],
            declarations: [
                NewTaskComponent,
                TaskListComponent
            ],
            providers: [
                { provide: ComponentFixtureAutoDetect, useValue: true },
                InfoMessagesService
            ]
        }).compileComponents();
        fixture = TestBed.createComponent(TaskListComponent);
        el = fixture.debugElement.nativeElement;

        component = fixture.componentInstance;
        // tasks array example:
        component.tasks = [
            {
                id: 1,
                startDate: '2017-07-02',
                time: 30,
                deadline: '2017-08-01',
                done: false,
                name: 'test remove',
                happy: true
            },
            {
                id: 2,
                startDate: '2017-08-02',
                time: 31,
                deadline: '2017-08-03',
                done: false,
                name: 'test done',
                happy: true
            }];
        initialNrOfTasks = component.tasks.length;
    }));

    it('should have a delete button', async(() => {
        expect(el.querySelectorAll('button#delete-task-1').length)
            .toBeGreaterThan(0);
    }));

    it('should have a done button', async(() => {
        expect(el.querySelectorAll('button#done-task-2').length)
            .toBeGreaterThan(0);
    }));

    it('should have a startDate column', async(() => {
        // second column header
        expect(el.querySelector('table.table thead tr th:nth-child(2)').textContent)
            .toEqual('start');

        // first line, second column
        expect(el.querySelector('table.table tbody tr td:nth-child(2)').textContent)
            .toContain('2017-07-02');
    }));

    it('should have a deadline column', async(() => {
        // third column header
        expect(el.querySelector('table.table thead tr th:nth-child(3)').textContent)
            .toEqual('deadline');

        // first line, third column
        expect(el.querySelector('table.table tbody tr td:nth-child(3)').textContent)
            .toContain('2017-08-01');
    }));

    it('should delete a task when delete button is clicked', async(() => {
        const http = fixture.debugElement.injector.get(Http);
        spyOn(http, 'delete').and.returnValue(Observable.of('DELETED'));
        spyOn(http, 'get').and.callFake(function() {
            if (http.get.calls.count() === 0) {
                    return Observable.of(JSON.stringify(component.tasks));
            } else if (http.get.calls.count() === 1) {
                    return Observable.of(JSON.stringify(component.tasks.splice(0, 1)));
            }
        });

        expect(el.querySelectorAll('.task-item').length).toEqual(initialNrOfTasks);

        // click on the delete button of the first task
        fixture.debugElement.query(By.css('button#delete-task-1'))
            .triggerEventHandler('click', null);
        fixture.detectChanges();

        expect(http.delete).toHaveBeenCalledWith('../tasks/1');
        expect(http.get).toHaveBeenCalledWith('../tasks');
        expect(el.querySelectorAll('.task-item').length).toEqual(initialNrOfTasks - 1);
    }));

    it('should mark a task as done when done button is clicked', async(() => {
        const http = fixture.debugElement.injector.get(Http);
        spyOn(http, 'put').and.returnValue(Observable.of('UPDATED'));
        spyOn(http, 'get').and.callFake(function() {
            if (http.get.calls.count() === 0) {
                    return Observable.of(JSON.stringify(component.tasks));
            } else if (http.get.calls.count() === 1) {
                    return Observable.of(JSON.stringify(component.tasks.splice(0, 1)));
            }
        });

        expect(el.querySelectorAll('.task-item').length).toEqual(initialNrOfTasks);

        fixture.debugElement.query(By.css('button#done-task-2'))
            .triggerEventHandler('click', null);
        fixture.detectChanges();

        expect(http.put).toHaveBeenCalledWith('../tasks/2',
            '{"id":2,"startDate":"2017-08-02","time":31,"deadline":"2017-08-03","done":true,"name":"test done","happy":true}');
        expect(http.get).toHaveBeenCalledWith('../tasks');
        expect(el.querySelectorAll('.task-item').length)
            .toEqual(initialNrOfTasks - 1);    // will take the task
                                              // out of the list
                                              // for now
    }));
});
